# MQTT binding

Load `binding-mqtt` separately from `chipi`. Register `bindings/mqtt/binding-mqtt.asd`
with ASDF when loading from a checkout. The core system does not depend on MQTT.
Broker settings belong to the application; this library never reads environment files.

```lisp
(binding-mqtt:mqtt-init :host "broker" :port 1883 :user "user" :pass "password")

(hab:defitem 'temperature "Temperature" 'float
  (binding-mqtt:mqtt-binding :topic "zigbee2mqtt/temp-sensor" :field "temperature"))
(hab:defitem 'humidity "Humidity" 'float
  (binding-mqtt:mqtt-binding :topic "zigbee2mqtt/temp-sensor" :field "humidity"))
(hab:defitem 'light "Light" 'string
  (binding-mqtt:mqtt-binding :topic "zigbee2mqtt/light" :field "state"
                             :publish-topic "zigbee2mqtt/light/set" :publish-field "state"))
(hab:set-item-value 'light "ON")
```

`mqtt-binding` also accepts `:qos` (default 0), `:retain` (default NIL), `:decode`
and `:encode`. Omit `:topic` for output only; omit `:publish-topic` for input only.
Without `:field`, the incoming value is the complete parsed JSON. Without
`:publish-field`, the outgoing value is serialized as a complete JSON value.
`encode` runs before wrapping `publish-field`. Incoming updates never publish back.
JSON booleans map to `item:true` and `item:false`. Missing fields, null, invalid JSON
and decoding failures keep the previous item value and publish sanitized diagnostics.

`mqtt-binding-received-at` records receipt independently of item change events.
A late binding receives the last parsed snapshot and its original receipt time.
The registry parses each message once per subscribed topic and releases registrations
on item redefinition and `binding:destroy`.

Use `mqtt-subscribe` for each raw `(topic payload)` message, including identical
repeats. It returns a unique subscription token; `mqtt-unsubscribe` removes only
that consumer. Raw handlers run on the event loop and must finish promptly.

`mqtt-publish` queues a string or string-keyed dictionary asynchronously; its return
value does not confirm delivery. `mqtt-publish-confirmed` uses QoS 1 and returns
`:acknowledged` only after PUBACK. Run it from an application worker, never an item
actor or the MQTT event loop. `mqtt-timeout`, `mqtt-disconnected` and
`mqtt-publication-error` distinguish sanitized failures. Stale publications cannot
replay on a replacement connection. The library has no durable command queue.
PUBACK does not confirm device action or physical state.

One managed connection exists per process. Repeating `mqtt-init` with identical
settings reuses it; different settings require `mqtt-shutdown` first. Reconnect
restores active subscriptions. `mqtt-health-snapshot` reports `connected`,
`loop_alive` and `heartbeat_age` separately. Binding reachability changes use the
standard Chipi binding events. `mqtt-shutdown` closes the loop and joins workers;
call it outside the event-loop thread. It also runs from Chipi's shutdown hook.

Run `(asdf:test-system :binding-mqtt)` for deterministic FiveAM tests. Load
`integration.lisp`, then call `binding-mqtt::run-mqtt-integration` for an isolated
local Mosquitto test including broker restart. This harness starts and stops its own
broker on a random local port and requires the `mosquitto` executable.

The temporary `make-mqtt-input`, `subscribe-on`, `publish-to`, `publish-and-wait`
and related compatibility entry points support migration of existing applications.
The legacy QoS 0 `publish-and-wait` returns `:sent` after local client acceptance;
it does not provide the PUBACK contract of `mqtt-publish-confirmed`.
