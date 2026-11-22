# MQTT Binding Plugin — Plan

Status: built and locally tested, 2026-11-22 (`binding-mqtt` 0.1.0).
The transport and item binding are implemented in `bindings/mqtt/`; its
`README.md` describes how to load and use them. FiveAM passed 89 checks,
and an isolated Mosquitto run proved PUBACK delivery, independent consumers,
subscription recovery after broker restart and worker shutdown. The local
smart-home integration also passed its stand, monitoring and browser checks.
Publishing the dependency and deployment to real equipment remain separate steps.

## 1. Role and shape

Chipi becomes an MQTT client through a separate `binding-mqtt` ASDF system,
alongside `binding-knx` and `binding-matter`. Loading `chipi` alone must not
load MQTT libraries. The package is `chipi.binding.mqtt`, with the nickname
`binding-mqtt`.

Two interfaces share the same transport:

- Raw subscriptions and publication functions for application code that must
  see every message, including repeated identical sensor readings.
- An item binding that selects values from incoming JSON and optionally
  publishes item commands to another topic.

The transport comes from smart-home. Leak detection, SQLite storage, audit,
the durable command queue and absolute valve neutralization deadlines stay
in the application. A local item change, broker PUBACK or relay channel
report must not imply a known physical valve position.

The house layer stays binding-neutral: MQTT configuration belongs in the
binding supplied to an item or house definer, not in `house.lisp`.

## 2. What the transport provides

`cl-mqtt` supplies MQTT packets and publication promises on a `cl-async`
event loop. A QoS 1 publication's promise resolves on PUBACK. Blackbird
connects those promises to the transport's request state. The plugin owns
connection lifetime, subscriptions, queue dispatch and health reporting.

The first version has one managed connection per process. Broker settings
are explicit arguments; the library never reads `.env` or environment
variables. Applications retain responsibility for configuration and secrets.

The temporary compatibility API preserves existing smart-home entry points
while the application moves to the managed interface. In particular, legacy
QoS 0 `publish-and-wait` means local client acceptance (`:sent`), whereas
`mqtt-publish-confirmed` means broker acknowledgement (`:acknowledged`).
These contracts must remain distinct.

## 3. Architecture

### 3.1 Layers

```
hab:defitem / house definers      application safety and command workers
        |                                      |
  mqtt-binding                    raw subscribe / publish functions
        |                                      |
  topic registry ---------------- managed MQTT transport
  parsed snapshots                command queue, subscriptions, request state
                                               |
                                  cl-mqtt / Blackbird / cl-async
                                               |
                                           MQTT broker
```

The binding registry has one raw subscription per active binding topic.
It parses each incoming JSON once and distributes the parsed snapshot to
all bindings on that topic. Independent raw consumers share the transport's
wire subscription without sharing their removal tokens.

### 3.2 Threads and ownership

One worker runs the MQTT event loop. Application threads enqueue transport
commands; the event loop performs MQTT operations and dispatches raw
handlers with `(topic payload)`. Handlers must finish promptly.

Incoming item updates use `item:set-value` with `:push nil`; outgoing item
pushes enqueue publications without waiting for the broker. A blocking
confirmed publication belongs on an application worker. Calling it from
the MQTT event loop or a Sento actor signals a publication error.

A separate watchdog checks connection state, worker liveness and the age
of the event-loop heartbeat. It can detect a stalled loop even while the
MQTT client object still exists. Health changes publish the standard
binding reachability event.

### 3.3 Connection lifetime

```lisp
(mqtt-init :host "broker" :port 1883 :user "user" :pass "password")
(mqtt-shutdown)
```

`mqtt-init` returns the managed transport object. Defaults are `localhost`,
port 1883 and no user or password. Repeating it with identical settings
keeps the same worker and subscriptions. Changing settings requires
`mqtt-shutdown` first.

A connection becomes available only after an accepted CONNACK. A failed
initial connection and a later disconnect both enter the reconnect loop.
A new connection restores the active topic set without duplicate wire
subscriptions. Commands tied to an old connection cannot run on its
replacement; the plugin does not replay application commands.

`mqtt-shutdown` clears managed subscriptions and pending commands, closes
the event loop and joins its worker and watchdog. It also releases binding
registrations and runs from Chipi's shutdown hook. It must be called from
outside the event-loop thread, which cannot join itself.

## 4. Raw subscriptions and publication

### 4.1 Independent consumers

```lisp
(let ((subscription
        (mqtt-subscribe "zigbee2mqtt/temp-sensor"
                        (lambda (topic payload)
                          (record-reading topic payload)))))
  ;; Remove this consumer when the application no longer needs it.
  (mqtt-unsubscribe subscription))
```

The handler receives the actual topic and the original payload string.
Every matching message reaches every registered consumer, including
identical repeats. MQTT topic filters support `+` and `#`; a wildcard at
the root does not match `$` system topics implicitly.

Each registration returns a unique removal token. Removing one consumer
preserves other raw handlers and item bindings on the same topic. Removing
an unknown or already removed token has no effect. A handler failure is
isolated so the remaining consumers can still receive the message.

### 4.2 Delivery contracts

```lisp
(mqtt-publish "zigbee2mqtt/light/set" "{\"state\":\"ON\"}"
              :qos 0 :retain nil)

(mqtt-publish-confirmed "zigbee2mqtt/light/set" "{\"state\":\"ON\"}"
                        :retain nil :timeout 3)
```

Both functions accept a string or a dictionary with string keys. Strings
are sent as supplied; dictionaries are serialized as JSON. Serialization
failures must not expose payloads or credentials in conditions.

| Function | Contract |
|---|---|
| `mqtt-publish` | Enqueue an asynchronous publication; QoS defaults to 0 and retain to NIL. Returning does not acknowledge delivery |
| `mqtt-publish-confirmed` | Publish with QoS 1; return `:acknowledged` only after PUBACK on the original connection, within the request deadline |

The timeout defaults to three seconds. `mqtt-timeout`, `mqtt-disconnected`
and `mqtt-publication-error` distinguish expiry, connection loss and
publication failure. They derive from `mqtt-error` through the publication
error class and contain sanitized reports.

Expiry cancels the request, including one already removed from the command
queue but not yet published. A late PUBACK cannot revive a cancelled
request or finish a request on another connection. The library has no
durable queue and does not retry application commands after reconnect.
PUBACK acknowledges the broker, not device execution.

## 5. The binding DSL

```lisp
(defitem 'sensor-temp "Temperature" 'float
  (mqtt-binding :topic "zigbee2mqtt/temp-sensor" :field "temperature")
  :persistence '(:id :influx :frequency :every-change))

(defitem 'sensor-humidity "Humidity" 'float
  (mqtt-binding :topic "zigbee2mqtt/temp-sensor" :field "humidity"))

(defitem 'light-state "Light" 'string
  (mqtt-binding :topic "zigbee2mqtt/light" :field "state"
                :publish-topic "zigbee2mqtt/light/set" :publish-field "state"))

(set-item-value 'light-state "ON")
```

Keys of `mqtt-binding`:

| Key | Meaning |
|---|---|
| `:topic` | Incoming topic; omit it for output only |
| `:field` | Top-level incoming JSON field; omit it to take the complete JSON value |
| `:publish-topic` | Outgoing topic; omit it for input only |
| `:publish-field` | JSON field wrapping the outgoing value; omit it to serialize the complete value |
| `:qos` | Publication QoS, 0 by default |
| `:retain` | Publication retain flag, NIL by default |
| `:decode` | Optional incoming value conversion |
| `:encode` | Optional outgoing conversion, applied before wrapping `:publish-field` |

At least one direction is required. The outgoing side uses the same
`mqtt-publish` function as applications. `binding:call-push-p` controls
item pushes; incoming updates always use `:push nil`. Repeated commands
use Chipi's existing `:push :always` mechanism.

### 5.1 Values and diagnostics

JSON `true` and `false` become `item:true` and `item:false`, including
inside complete JSON objects and arrays. A present `false` differs from a
missing field. Empty strings and numeric zero remain valid values.

| Incoming failure | Result |
|---|---|
| Missing selected field | Keep the previous item value; diagnose `:missing-field` |
| JSON null in the selected value | Keep the previous item value; diagnose `:null-value` |
| Invalid JSON or trailing non-whitespace data | Keep the previous value and cached snapshot; diagnose `:invalid-json` |
| Decode function signals an error | Keep the previous item value; diagnose `:decode-error` |
| Another value/update error | Diagnose `:invalid-value` |

Diagnostics use `mqtt-diagnostic-event` with topic and reason, without
embedding the payload. Null is unsupported rather than silently mapped to
NIL, false or zero.

### 5.2 Snapshots and receipt time

The active topic registry caches the last successfully parsed JSON and its
original receive time. A late binding on that topic receives that snapshot.
Field selection and decoding can still reject it for that binding; such
an error does not erase an existing item value.

`mqtt-binding-received-at` records successful receipt separately from the
item's change timestamp. A repeated identical value updates receipt
metadata even if it creates no item change event. Updates carry the
original receive time through `:timestamp`; sequence checks prevent an
older cached delivery from overwriting a newer message.

Destroying a binding or redefining an item removes the old registration.
Other bindings on the topic remain active. Removing the last binding
releases the registry entry and its raw subscription; independent raw
consumers still retain their own wire subscription.

## 6. Availability

`mqtt-health-snapshot` returns a dictionary with string keys:

| Key | Meaning |
|---|---|
| `connected` | An accepted MQTT connection exists |
| `loop_alive` | The worker is alive and its heartbeat is within the configured timeout |
| `heartbeat_age` | Monotonic seconds since the last loop tick, or NIL before the first tick |

The function accepts `:heartbeat-timeout` (three seconds by default), with
an optional explicit transport object for compatibility. An absent managed
transport reports unavailable. `binding:binding-reachable-p` requires both
connection and loop health, and is false for a destroyed MQTT binding.

Connection loss, a dead worker and stale sensor data must remain visible.
Sensor freshness is an application responsibility: safety code must use
raw subscriptions, not only `item-changed-event`, because identical
readings still prove receipt.

## 7. Files

```
bindings/mqtt/
├── binding-mqtt.asd       ; separate plugin and FiveAM test system
├── package.lisp           ; public package and compatibility exports
├── transport.lisp         ; managed connection, queue, subscriptions, PUBACK and health
├── binding.lisp           ; topic registry, conversions, item binding and diagnostics
├── binding-test.lisp      ; deterministic binding and transport contract tests
├── integration.lisp       ; isolated Mosquitto lifecycle and reconnect check
└── README.md              ; loading, API and migration contracts
```

The plugin depends on `chipi`, `cl-mqtt`, Bordeaux Threads, Serapeum,
Yason, log4cl-extras and trivial-types. MQTT dependencies belong to the
plugin ASDF system; none are added to `chipi.asd`.

## 8. Build order and verification

| Phase | Work | Proof / status on 2026-11-22 |
|---|---|---|
| 1 | Move the transport into a separate system, with explicit settings and lifecycle | Built; core Chipi loads without the `cl-mqtt` package; repeated managed init keeps one worker |
| 2 | Share raw subscriptions and add confirmed publication | Built; independent consumers, cancellation, distinct errors, old acknowledgements and reconnect generation guards tested |
| 3 | Add the item binding and topic snapshot registry | Built; two fields from one parse, all three binding directions, diagnostics, timestamps, redefinition and `:push :always` tested |
| 4 | Exercise a real local broker | Passed; Mosquitto receives QoS 1 publications, restarts, restores the remaining consumer and shuts down with both MQTT threads joined |
| 5 | Connect smart-home locally, then adapt MOS | Passed locally; raw safety input, durable delivery, hot-water-only MOS configuration and explicit channel `/get` payload tested |
| 6 | Publish and pin the dependency | Open; publish the Chipi branch separately, then pin the tested commit in smart-home and repeat verification without the local checkout override |

`(asdf:test-system :binding-mqtt)` runs 18 deterministic FiveAM tests,
89 checks at the recorded verification. They cover shared parsing,
missing/false/null/error distinctions, no input echo, encoding order,
receipt time, consumer removal, real item redefinition, managed lifecycle,
health options and sanitized publication conditions.

Loading `bindings/mqtt/integration.lisp` and calling
`binding-mqtt::run-mqtt-integration` starts and stops its own local
Mosquitto on a random high port. It does not require a household broker.
The existing Chipi suite passed 1353 checks against a byte-identical,
isolated source copy so its runtime cleanup could not delete user data.

The smart-home checks passed 59 stand tests and 17 monitoring tests, plus
browser scenarios for temperature persistence, leak submission, both
virtual relays, neutralization and mobile layout. They used an isolated
broker and SQLite; physical valve position stayed `unknown`.

## 9. Decisions and remaining boundaries

- One managed broker connection per process in the first version. Changing
  brokers requires explicit shutdown; multiple managed connections are not
  part of this implementation.
- No environment-file loading, durable storage, command replay, leak
  decisions or manual-override audit in the plugin. Those belong to the
  application and its protective core.
- Preserve legacy transport names during migration, while documenting the
  difference between local QoS 0 acceptance and QoS 1 PUBACK.
- Use Chipi's existing binding availability and command mechanisms rather
  than introducing MQTT-specific concepts into the house layer.
- Publishing, dependency pinning, deployment and real equipment checks are
  separate actions. Local validation does not establish physical valve state.
