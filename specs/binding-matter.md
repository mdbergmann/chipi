# Matter Binding Plugin — Plan

Status: plan, 2026-11-08. Nothing of this exists yet. cl-matter lives at
`../cl-matter`; its state is the one its README describes as of commit
`12a1f41` (controller examples: find, commission and control a real device).

## 1. Role and shape

Chipi plays two Matter roles with one plugin. As a **controller** it is the
commissioner and administrator of its own fabric, and Matter devices become
items (sections 2–5). As a **bridge** it is a Matter device itself that
Apple Home, Google Home or Alexa commission, and items become bridged
devices (section 9). Sections 1–8 are the controller role.

Chipi becomes a **Matter controller**: the commissioner and administrator of
its own fabric. Every Matter device (lamp, plug, contact, shutter, thermostat)
is a *node* of that fabric, with *endpoints* carrying *clusters* whose
*attributes* are state and whose *commands* are what one does to it.

How that compares to the two bindings we have or planned:

| | KNX binding | HomeKit spec | Matter binding |
|---|---|---|---|
| Chipi is | client of one gateway | server (bridge) | client of many nodes, plus commissioner |
| Address of a value | group address + DPT | AID.IID | node, endpoint, cluster, attribute / command |
| Connection | one tunnel, reconnect loop | inbound TCP | one CASE session per node, re-established on demand |
| State updates | bus listener | n/a | one subscription per node, reports → items |
| Setup ceremony | none | pairing PIN | commissioning (PASE, attestation, NOC, CommissioningComplete) |
| Persistent secrets | none | keypair, pairings | fabric keys, NOCs, node ids, resumption records |

Chipi is one more administrator next to Apple Home, Google Home, chip-tool: a
device that already has an administrator gets its commissioning window opened
there, and Chipi commissions through it. That is exactly how the Eve Energy
plug was done on 2026-11-07 with `examples/commission-device.lisp`.

The house layer stays binding-neutral (CLAUDE.md): `deflight`, `defplug`,
`defrollershutter` take a `matter-binding` form where they take a
`knx-binding` today. Nothing in `house.lisp` learns about Matter.

## 2. What cl-matter gives, what the plugin needs first

Everything the controller side needs is there and has been used against a real
device: discovery (`discover-commissionable-nodes`,
`discover-operational-node`), pairing codes (`decode-manual-pairing-code`,
`decode-qr-code`), `commission`, `establish-case-session`, `read-attributes`,
`write-attributes`, `invoke-command`, `subscribe-attributes`,
`commissioner-state` / `read-state-file` / `write-state-file`,
`request-commissioning-window`. All of them also come in a non-blocking
`start-...` form with `:on-complete`.

Gaps that the plugin runs into, in the order they bite:

1. **SBCL network backend** (`src/net/backend-sbcl.lisp`): IPv4 datagrams
   only; `open-udp-socket :family :ipv6` signals `not-supported`, and so do
   `multicast-join` and `network-interfaces`. Matter itself is IPv6-only and
   discovery wants multicast. On AmiLisp (macOS host) the backend is
   complete, and that is where the plug was commissioned. Decision
   (2026-11-09): the plugin is AmiLisp-first; cl-matter's plan item 4
   (`setsockopt` / `getifaddrs` through `sb-alien`) is deferred, and so are
   the plugin's network tests on SBCL. Everything that does not open a
   socket -- registry, conversions, actor message handling -- still has to
   run and be tested on SBCL.
2. **Single-threaded, polled exchange manager**: every cl-matter call that
   touches sessions or exchanges must happen on the thread that polls the
   manager. The plugin owns that thread (section 3.2). cl-matter needs no
   change for this, but the discipline is the plugin's.
3. **Cluster definitions**: beyond the commissioning clusters only
   Descriptor, Basic Information, Identify and On/Off are defined. Unknown
   attributes still read fine as raw TLV (`attribute-report-value` returns
   the value and a second value `nil`), so a generic binding works on day
   one; typed definitions give names, types and `command-fields`. They
   belong in cl-matter (`define-cluster`), not in the binding. Needed for
   the house kinds we have:

   | Cluster | Id | For |
   |---|---|---|
   | Level Control | 0x0008 | dimmable light |
   | Color Control | 0x0300 | colour light (later) |
   | Boolean State | 0x0045 | contact sensor (`defcontact`) |
   | Temperature Measurement | 0x0402 | `defheating` temperature |
   | Relative Humidity Measurement | 0x0405 | sensors |
   | Occupancy Sensing | 0x0406 | motion |
   | Window Covering | 0x0102 | `defrollershutter` |
   | Thermostat | 0x0201 | `defheating` setpoint and mode |
   | Switch | 0x003B | buttons; needs *events*, which cl-matter does not report yet |
   | Electrical Power / Energy Measurement | 0x0090 / 0x0091 | the Eve plug's measurements |

4. **Attestation**: the trust store has the SDK's test PAAs only; a real
   device needs `:attestation :ignore` (signature still checked against the
   DAC) until the Distributed Compliance Ledger's PAAs can be loaded with
   `add-trusted-paa`. The plugin exposes the switch and, later, a directory
   of PAA certificates.
5. **Session lifetime**: sessions never expire in cl-matter; a device that
   rebooted or moved address answers nothing. The plugin must detect a dead
   session (interaction timeout, subscription `:on-failure`) and re-resolve
   and re-establish, the way the KNX binding reconnects.
6. **Fabric label**: cl-matter's device side serves `UpdateFabricLabel`; the
   commissioner side should send it after commissioning so the device's
   other administrators list the fabric as "Chipi" and not as a bare id.
   Small addition to cl-matter.

## 3. Architecture

### 3.1 Layers

```
hab:defitem / house definers
        |
  matter-binding            one per item: node, endpoint, cluster, attribute | command,
        |                   value conversion
  matter controller         one per process: the controller actor, the commissioner and
        |                   its fabric, the node registry, sessions, subscriptions
  cl-matter                 discovery, PASE/CASE, interaction model, commissioner
  cl-matter/net             UDP v6, multicast (AmiLisp complete, SBCL pending)
```

### 3.2 The Matter actors

Each role has one Sento actor that owns one exchange manager, on a
`:pinned` dispatcher, which in Sento means a thread of its own with a
`message-box/bt`. Both live in chipi's item actor system (`isys:ensure-isys`),
where the KNX binding also registers its verify dispatcher.

```
matter-controller   pinned actor, state: exchange manager (ephemeral port), secure channel,
                    interaction model, commissioner, sessions, subscriptions, node registry
matter-bridge       pinned actor, state: exchange manager (port 5540), secure channel,
                    interaction model, device, bridge node
:tasks dispatcher   discovery (`dns-sd:browse`), file writes -- the 4-worker pool isys has
```

Why an actor and not a thread with a queue: the queue *is* a mailbox, the
"return a future" *is* `act:?`, and chipi's rule is that state lives in an
actor and is touched only from its receive. The exchange manager is state
of exactly that kind. What the actor shape costs is one idiom: cl-matter
wants to be polled, and an actor runs only when a message arrives. So the
actor polls itself:

```lisp
(:poll)   -> (exchange-manager-poll manager :timeout 0.05)   ; wait for a datagram, run timers
             (act:! act:*self* '(:poll))                       ; and again
```

The `:poll` message is always queued behind whatever else arrived, so a
request waits at most one poll, 50 ms, and runs in order with the
datagrams. Everything cl-matter calls back -- `:on-complete` of a read,
`:on-report` of a subscription, a cluster's command handler on the bridge
-- runs inside `exchange-manager-poll`, that is inside receive, in actor
context. From there the only way out is a tell to an item
(`item:set-value`), which never blocks.

Messages into the controller actor:

| From | Message | Returns |
|---|---|---|
| an item actor's push (`push-fun`) | `(:invoke node endpoint cluster command fields)` | nothing; `act:!`. The item actor must not wait on the Matter actor |
| `pull-fun` | `(:read node paths)` | a future from `act:?`, which `binding:exec-pull` already accepts |
| REPL, wizard, API | `(:commission code label)`, `(:decommission node)`, `(:open-window node)`, `(:set-label node label)` | a future; the commissioning job's `on-step` tells the wizard as it goes |
| itself | `(:poll)`, `(:reconnect node)` on the backoff | |
| the `:tasks` pool | `(:discovered nodes)` after a scan | |

Into the bridge actor: `(:set-attribute endpoint cluster attribute value)`
from a bridged item's push, `(:add-endpoint ...)` / `(:remove-endpoint ...)`
when bindings come and go, `(:open-window)`, `(:remove-fabric index)` from
the UI, and `(:poll)`.

Two actors rather than one because the two roles share no cl-matter state
(two transports, two fabrics, two session managers) and because the pure-
Lisp crypto is slow: a CASE handshake or a PBKDF2 of 1000 iterations takes
hundreds of milliseconds, and Apple Home marks a bridge "No Response" when
its subscription reports miss the max interval. A commissioning in the
controller must not stall the bridge, nor a hub's handshake delay a light
switched through the controller. The mDNS responder of the bridge keeps the
thread cl-matter gives it; the controller's queries are synchronous
`dns-sd:browse` calls and run as tasks on the `:tasks` dispatcher, never in
either actor.

An error inside a poll is caught in receive and logged; the loop goes on,
because one bad datagram must not end the actor. `hab:shutdown` stops both
actors through the shutdown hook, which ends their loops, closes their
managers and writes the state files.

### 3.3 Node registry

A file in the configured storage directory, next to the commissioner's state
file. One entry per commissioned node:

| Field | From |
|---|---|
| node id | the commissioner hands it out |
| label | given at commissioning; the name the DSL uses |
| vendor id, product id, vendor name, product name, serial / unique id | Basic Information read after commissioning |
| endpoints: id, device types, server clusters | Descriptor wildcard read after commissioning |
| last known addresses and port | operational discovery, refreshed on each resolve |
| commissioned at, last seen | housekeeping |

The registry is what discovery, labeling and code generation (section 5) work
on. The commissioner's own state (fabric, keys, next node id, resumption
records) stays in cl-matter's state file, written through
`write-state-file` after every change.

### 3.4 Sessions and subscriptions

Per node the controller keeps: registry entry, current CASE session or nil,
the subscription or nil, a reconnect backoff like `*reconnect-backoff-secs*`
in the KNX binding.

- A session is opened lazily on the first request to a node, and eagerly at
  init for every node that has bindings, so subscriptions start early.
- All attribute paths bound on a node go into one subscription
  (`start-subscribe`, `:min-interval 0 :max-interval 60`). Each report is
  dispatched through a registry `(node endpoint cluster attribute) →
  bindings`, the counterpart of the KNX `*ga-binding-registry*`, and set on
  the bound items with `:push nil` so the value is not echoed back.
- On `:on-failure` or a timed-out interaction the session is dropped, the
  node re-resolved with `discover-operational-node` (Thread devices change
  addresses) and re-established on the backoff; then re-subscribed. A node
  that fails for good is marked unreachable in the registry, which the UI
  and an optional status binding can show.
- Pushes to an unreachable node: a *state* (light on) is recorded and sent
  when the node is back; a *command* (shutter stop) is dropped. Same policy
  and same key as the KNX binding: `:repush-on-reconnect`.

### 3.5 The binding DSL

```lisp
(defconfig "my-home"
  (matter-init :storage "~/.chipi/matter/"     ; commissioner.state and nodes
               :fabric-id #xC1 :vendor-id #xFFF1
               :attestation :ignore              ; :verify once PAAs are loaded
               :interface "en0"))                ; mDNS and link-local scope

;; a plug: the On/Off cluster is state (attribute 0) and command (On/Off) in one
(defplug plug-kitchen "Kitchen plug"
  (matter-binding :node "eve-plug-kitchen" :endpoint 1 :cluster :on-off))

;; a dimmable light: two items on the same endpoint
(deflight light-living "Living room"
  (matter-binding :node "hue-living" :endpoint 1 :cluster :on-off))
(defitem 'light-living-level "Living room level" 'integer
  (matter-binding :node "hue-living" :endpoint 1 :cluster :level-control
                  :attribute :current-level
                  :command (:move-to-level-with-on-off :level (:scale 254/100) :transition-time 0)))

;; a sensor: read-only, a scaled float
(defitem 'temp-bath "Bath temperature" 'float
  (matter-binding :node "aqara-bath" :endpoint 1 :cluster :temperature-measurement
                  :attribute :measured-value :scale 1/100 :call-push-p nil))

;; a contact
(defcontact window-bath "Bath window"
  :tags '((:ui-readonly . t))
  (matter-binding :node "aqara-bath" :endpoint 2 :cluster :boolean-state :attribute :state-value))

;; a shutter: the house definer, each channel its own binding
(defrollershutter blind-living "Blind living room"
  :move     ((matter-binding :node "shelly-blind" :endpoint 1 :cluster :window-covering
                             :command (:up-or-open :down-or-close) :repush-on-reconnect nil))
  :stop     ((matter-binding :node "shelly-blind" :endpoint 1 :cluster :window-covering
                             :command :stop-motion :repush-on-reconnect nil))
  :position ((matter-binding :node "shelly-blind" :endpoint 1 :cluster :window-covering
                             :attribute :current-position-lift-percent-100ths :scale 1/100
                             :command (:go-to-lift-percentage :lift-percent-100ths (:scale 100)))))
```

Keys of `matter-binding`:

| Key | Meaning |
|---|---|
| `:node` | registry label (string) or node id (integer). Label recommended: it survives re-commissioning |
| `:endpoint` | endpoint id |
| `:cluster` | cluster name (keyword) or id |
| `:attribute` | attribute that is the item's state; default per cluster (`:on-off` → `on-off`) |
| `:command` | what a push does: a command name, `(name field (:scale r) ...)` for a command with fields, or a pair `(false-command true-command)` for a boolean item. Default per cluster (`:on-off` → `off` / `on`) |
| `:scale` | rational applied to the attribute value on the way in and inverted on the way out |
| `:call-push-p`, `:initial-delay`, `:delay`, `:repush-on-reconnect` | as in the KNX binding; `:initial-delay` reads the attribute once, `:delay` polls it (subscriptions make both unnecessary for a node that supports them) |

Value conversion lives in one table keyed by cluster and attribute type:
`:bool` ↔ `item:true` / `item:false`, integers direct, `:scale` for the
hundredths Matter likes, `:null` → no change. Beyond that the binding is
deliberately dumb: a cluster it does not know is still addressable by ids.

## 4. Detected devices, commissioning, labeling

### 4.1 Discovery

`(matter-discover)` returns what is on the network right now, from the worker
thread:

- **commissionable** nodes (`_matterc._udp`): discriminator, vendor and
  product id, device name (TXT `DN`), device type (`DT`), pairing hint and
  instruction (`PH`, `PI`), commissioning mode, addresses. These are the
  devices whose window is open: new ones, or ones another administrator put
  into pairing mode.
- **operational** nodes of our fabric (`_matter._tcp`, our compressed fabric
  id), matched against the registry: which of our nodes are on the air.
- **known but silent**: registry nodes that did not answer.

Discovery takes a few seconds per service type (`:timeout 3..4`) and is on
demand only, never periodic; the per-node re-resolve of section 3.4 is the
only background use of mDNS.

### 4.2 Commissioning

```lisp
(matter-commission "3497-011-2332" :label "kitchen-lamp")     ; manual pairing code
(matter-commission "MT:-24J0AFN00KA0648G00" :label "...")      ; QR payload
```

Steps, all in the controller actor except the discovery:

1. Decode the code (`decode-manual-pairing-code` / `decode-qr-code`): passcode,
   discriminator (short for a manual code).
2. Find the node (`discover-commissionable-nodes :short-discriminator`). None
   found: "window not open" is the answer the user needs. Several: take the one
   whose vendor/product match the code's, else ask.
3. `start-commissioning` with the passcode and the configured attestation
   policy; `commission` ran the Eve plug in well under 90 seconds, so the
   future gets that timeout.
4. After success: read Basic Information and the Descriptor of every endpoint
   (as `commission-device.lisp` does) into the registry entry; write the
   label to the device's NodeLabel (Basic Information attribute 5, `manage`
   privilege, which the commissioner's ACL entry has); send
   `UpdateFabricLabel "Chipi"`; save commissioner state and registry.
5. Open the CASE session and subscribe for whatever bindings already name
   that label — a `defitem` may well be written before the device is
   commissioned.

Companion functions:

- `(matter-decommission node)`: `RemoveFabric` of our fabric index on the
  device, drop the session, forget the registry entry. The device stays with
  its other administrators.
- `(matter-open-window node &key timeout)`: `request-commissioning-window`,
  returns the manual pairing code and QR string to hand to the next
  administrator. The reverse of how Chipi got in.
- `(matter-describe node)`: the registry entry readably, and for each endpoint
  the house definer its device type suggests (section 5).

Failure modes to report plainly: no commissionable node; attestation failed
(only with `:verify`); `BUSY_WITH_OTHER_ADMIN`; timeout at a named step
(`commissioning-error-step`); a device at its fabric limit.

Where it is offered, in order: REPL functions first (that is how the KNX
binding is used too), then the UI's pairing wizard (section 4.4), then the
API (`POST /matter/commissionings` and `GET /matter/nodes`, a scope of
their own, result through SSE because it takes long) for clients outside
the process. The UI does not go through the API: chipi-ui runs in the same
image and calls the controller directly.

Every one of these is a thin shell over one asynchronous controller
operation, so the controller exposes commissioning as an object the shells
can watch:

```lisp
(matter-start-commissioning code &key label on-step on-complete)
;; => a COMMISSIONING-JOB; its state is :discovering, :pairing, :attesting,
;;    :credentials, :completing, :reading, :labeling, :done or :failed
```

`on-step` is called with the job on every state change, `on-complete` once
with the result or the error. The REPL function waits on it; the wizard and
the SSE stream render it as it goes.

### 4.3 Labeling and marking

Three places carry a name, and they must not be confused:

1. **Registry label** — Chipi's name for the node, the key of `:node` in the
   DSL. Set at commissioning, changed with `(matter-set-label node "...")`.
   Changing it re-points the bindings that use it.
2. **Device NodeLabel** — what other administrators see. Written from the
   registry label at commissioning and on `matter-set-label`, unless
   `:device-label nil`.
3. **Item label** — the usual `defitem` label; untouched.

Marking items as Matter-based: a binding cannot add tags to its item
(`defitem` builds the bindings before it makes the item), so the controller
answers the question the other way round — `(matter-node-of item)` and
`(matter-items-of node)` from the binding registry. The UI uses the first for
a small badge and an unreachable mark on the card row; the API adds `matter`
to an item's JSON where the controller knows it. Users who want a tag can
still give `:tags '((:matter . "kitchen-lamp"))` themselves; the plugin does
not need it.

Reachability as a value: `(matter-binding :node "x" :attribute :reachable)`
on the root endpoint's Basic Information gives a boolean item that follows
the node's reachability, and rules can act on it.

### 4.4 The UI: devices view and pairing wizard

#### How it plugs in

chipi-ui knows nothing about Matter, and must not come to depend on the
binding (CLAUDE.md's rule for the house layer applies here too). What it
gets is one general extension point, and the binding ships a small system
of its own that uses it:

```
chipi-ui                     settings sections registry + sub-views under /settings/...
binding-matter/ui            registers "Matter devices": the devices view and the wizard
binding-matter               the controller the view calls
```

The extension point in chipi-ui, usable by any plugin:

```lisp
(ui-settings:register-section
  :matter                              ; id, also the path: /settings/matter
  :label "Matter devices"
  :render #'render-matter-devices)     ; (ctx container) -> renders into container
```

- `render-settings` renders a card per registered section below the
  built-in ones, each a row that navigates to `/settings/<id>`. A section
  with nothing to show offers `:summary`, a function returning the one line
  of the card ("3 devices, 1 unreachable").
- `%dispatch-path` matches `/settings/<id>` and renders the section's view
  with the same header, back button and PIN lock as the settings view: a
  locked device asks for the PIN before any of this, and the access-key gate
  stands in front of everything as it does now.
- The section's path is registered as a CLOG route like a page's, so a deep
  link or reload lands in it.

That is a handful of lines in `settings.lisp`, `page-renderer.lisp` and
`main.lisp`, all binding-neutral, with their own tests.

#### Live updates from the controller

The whole UI is server-side CLOG: the Lisp side holds the DOM and pushes
changes over the websocket, and it already does so from other threads (the
item-change-listener actor updates item rows). A wizard whose steps happen
in the controller actor therefore needs no polling and no SSE: the
`on-step` callback of the commissioning job updates the view's progress
element directly, through the same guard the item rows use
(`%owner-alive-p`: the connection, or the view, may be gone by the time the
step arrives). A view that is left mid-pairing does not cancel the pairing;
coming back shows the job's current state from the controller.

Discovery is the one slow synchronous thing (3–4 seconds per service type,
a task on the `:tasks` dispatcher). The view shows "Scanning…" and fills the list when
the worker calls back; a second scan while one runs is ignored.

#### The devices view (`/settings/matter`)

Three cards, top to bottom:

**Ready to pair.** The commissionable nodes of the last scan, with a
"Scan" button and the time of the last scan. One row per node:

| Shown | From the advertisement |
|---|---|
| name | TXT `DN`, else "Vendor 0x1234 product 0x5678" |
| what it is | TXT `DT` through the device-type table of section 5 ("On/Off Light") |
| how to pair | TXT `PH` / `PI`: the pairing hint bits say "press the button", "scan the QR on the device", and `PI` is the free-text instruction; shown when present |
| state | `CM` 1 or 2: window open (basic / enhanced). Only those are listed, the rest are not pairable right now |
| where | first address and port, for the curious |

Tapping a row opens the wizard for it. Below the list, a hint for the
device that does not appear: a new device opens its window by itself when
powered; one that already belongs to Apple Home or Google Home gets the
window opened there ("pairing mode" / "turn on pairing mode"), which also
shows the code to type into the wizard. The hint does the job the pairing
code's own packaging would.

**Paired.** The registry: one row per node with label, product, endpoint
count and reachability (from the controller's sessions, no scan). Tapping
a row opens the node view: rename (registry label and, by default, the
device's NodeLabel), **Identify** (the Identify cluster on each endpoint
that has it, so the lamp blinks and one knows which one it is), **Open
window for another app** (shows the manual code and the QR as text, with
the minutes left), **Remove** (decommission, with a confirmation). Then its
endpoints, each with device type, clusters and the items bound to it, and
for an endpoint no item binds, the suggested definer from section 5 as
copyable code.

**Controller.** Fabric id, vendor id, attestation policy, storage path,
the state of both actors. Read-only; configuration stays in `matter-init`.

#### The pairing wizard

Four steps in one view, each replacing the last, a back arrow to the
devices view throughout.

1. **Device.** The row tapped in *Ready to pair*, or, when the wizard was
   opened from the "Pair with a code" button instead, nothing yet.
2. **Code and label.** A text field for the manual pairing code (11 or 21
   digits, dashes allowed) or a QR payload (`MT:…`). Decoded locally the
   moment it is complete: Verhoeff check, discriminator, vendor and
   product. A code whose short discriminator matches the chosen device
   is marked as such; a mismatch is said plainly ("this code is for
   another device, discriminator 15, this one advertises 9") and the
   wizard offers to look for the device the code belongs to. A label
   field, prefilled with `DN` or the product name. Camera QR scanning
   (the browser's `BarcodeDetector`) is a later nicety; the field takes
   the string either way.
3. **Pairing.** The job's states as a checklist that ticks itself:
   finding the device, establishing the pairing session (PASE),
   checking the device's identity (attestation), issuing credentials,
   completing, reading what the device is, labeling. Each state has a
   plain-language line, and the failures of section 4.2 map to one
   each ("The device is being paired by another app, try again in a
   minute" for `BUSY_WITH_OTHER_ADMIN`; "The device did not answer. Is
   its pairing window still open?" for a timeout at the PASE step). A
   failed step offers "Try again", which starts a new job with the same
   code.
4. **Done.** The node view from *Paired*: what the device is, an
   Identify button to confirm it is the right lamp, and the suggested
   item definitions as copyable code, since defining items is still a
   matter of the configuration script. "Done" returns to the devices
   view, where the node now sits under *Paired* and is gone from *Ready
   to pair*.

#### Marks on the item cards

Outside the settings, the itemgroup cards show two small things for a
Matter-bound item: a "Matter" badge next to the kind badge, and a muted
row with an "unreachable" mark while the node is down. The renderer must
not ask the binding, so the primitive offers one general thing the binding
fills in: a generic `binding:binding-badge` (default `nil`) and an exported
reader for an item's bindings; the renderer shows whatever badges an item's
bindings return and a binding's reachability through a second generic,
`binding:binding-reachable-p` (default `t`). The KNX binding can answer
these too, one day, with its tunnel state.

## 5. From device types to house definers

`matter-describe` closes the loop between "detected" and "defined": for each
endpoint of a commissioned node it prints a ready-to-paste form.

| Device type | Id | Suggested form |
|---|---|---|
| On/Off Light | 0x0100 | `deflight` on `:on-off` |
| Dimmable Light | 0x0101 | `deflight` plus a level item on `:level-control` |
| Color Temperature / Extended Color Light | 0x010C / 0x010D | as dimmable, colour later |
| On/Off Plug-in Unit | 0x010A | `defplug` |
| Contact Sensor | 0x0015 | `defcontact` on `:boolean-state` |
| Temperature Sensor | 0x0302 | `defitem` float, `:temperature-measurement :scale 1/100` |
| Humidity Sensor | 0x0307 | `defitem` float, `:relative-humidity-measurement :scale 1/100` |
| Occupancy Sensor | 0x0107 | `defitem` boolean, `:occupancy-sensing` |
| Window Covering | 0x0202 | `defrollershutter` with the three bindings of section 3.5 |
| Thermostat | 0x0301 | `defheating`: local temperature, occupied heating setpoint (`:scale 1/100`), system mode as `defmode` |
| Generic Switch | 0x000F | `defpulse` — needs event reporting in cl-matter first |

The table lives in the plugin as data; it is also what the UI's settings view
shows next to each endpoint.

## 6. Files

```
bindings/matter/
├── binding-matter.asd
├── matter-actors.lisp        ; the two pinned actors, the poll loop, message protocol
├── matter-registry.lisp      ; node registry: entries, file, label lookup
├── matter-controller.lisp    ; commissioner and fabric, sessions, subscriptions,
│                             ; reconnect, discover / commission / decommission / window
├── matter-convert.lisp       ; cluster and attribute value conversion, device-type table
├── binding-matter.lisp       ; matter-binding class and macro, matter-init, matter-shutdown
├── binding-matter-ui.lisp    ; system binding-matter/ui: the devices view and the wizard,
│                             ; registered as a chipi-ui settings section
├── binding-matter-test-suite.lisp
├── binding-matter-test.lisp       ; unit: conversion, registry, binding with a mocked controller
├── binding-matter-integtest.lisp  ; in-process cl-matter lamp over loopback: commission,
│                                  ; subscribe, switch, decommission
├── binding-matter-ui-test.lisp    ; the view and wizard against a mocked controller, with
│                                  ; chipi-ui's clog-test-utils
├── example-script.lisp
└── README.md
```

```lisp
(defsystem "binding-matter"
  :depends-on ("chipi" "cl-matter" "bordeaux-threads")
  ...)

(defsystem "binding-matter/ui"
  :depends-on ("binding-matter" "chipi-ui")
  :components ((:file "binding-matter-ui")))
```

A headless Chipi loads `binding-matter` alone; one with the web UI loads
`binding-matter/ui` as well, which registers its settings section on load.
chipi-ui itself gains only the section registry and the `/settings/<id>`
routing, nothing Matter-specific.

cl-matter is not on OCICL; it is loaded from `~/quicklisp/local-projects` (or
a sibling checkout via `asdf:load-asd`), as the KNX binding's README does for
chipi itself.

The integration test is the plugin's strongest asset: cl-matter has a device
side (`make-device`, `examples/lamp-device.lisp`), and
`test/commissioning-test.lisp` already commissions one over loopback UDP in a
single process. The plugin's integtest does the same with a real
`defitem`: no hardware, no network, deterministic.

## 7. Build order

| Phase | Work | Done when |
|---|---|---|
| 0 | cl-matter: `UpdateFabricLabel` from the commissioner; Level Control, Boolean State, Temperature Measurement, Window Covering clusters (SBCL IPv6 + multicast + interfaces deferred, see section 8) | new clusters have tests in cl-matter |
| 1 | `matter-actors`, `matter-registry`, controller core: commissioner state, CASE per node, read / invoke as futures | integtest: a `defitem` on an in-process lamp reads and switches it |
| 2 | `matter-binding` with subscriptions, `matter-init` / `matter-shutdown`, README | the Eve plug is a `defplug` in a running Chipi; UI toggles it; Apple Home's change shows up |
| 3 | `matter-discover`, the commissioning job with its states, `matter-commission`, `matter-decommission`, `matter-open-window`, labels, `matter-describe` | a new device goes from pairing code to item without leaving the REPL |
| 3b | chipi-ui: settings section registry and `/settings/<id>` routing; `binding-matter/ui`: devices view and pairing wizard over the phase-3 functions | a device is paired from the wall tablet, and its suggested definer copied from the node view |
| 4 | resilience: re-resolve, re-establish with backoff, re-subscribe, unreachable flag, repush policy | pull the plug's power, plug it back: Chipi follows without a restart |
| 5 | remaining clusters and conversions; `defrollershutter`, `defheating`, sensors end to end | one device per house kind on the bench |
| 6 | item-card badges and the unreachable mark (the two binding generics); API routes and SSE for clients outside the process | a phone app or script can pair a device too |

## 8. Decisions to take

- **Platform first**: decided, AmiLisp now. The SBCL network backend comes
  later; until then only the socket-free parts of the plugin are tested on
  SBCL, the rest on AmiLisp against an in-process cl-matter device.
- **Node reference**: label (recommended) with integer id as fallback.
- **Attestation default**: `:ignore` until PAAs can be loaded; make `:verify`
  the documented goal and provide `:paa-directory`.
- **One fabric per Chipi**: yes; fabric id configured once, vendor id
  `#xFFF1` (the test range — fine for a private fabric, not for a certified
  product).
- **Sleepy devices**: Thread sensors advertise `SII` / `SAI`; interaction
  timeouts per node should come from them, not from one global default.
- **Events**: cl-matter does not report events yet; buttons and switches
  wait for that.
- **Items from the UI**: the wizard ends with suggested `defitem` forms to
  paste into the configuration script; it does not create items itself.
  Items defined from the UI would need them persisted outside the script,
  which chipi has no notion of today. A separate decision, not a Matter one.
- **Scanning**: on demand from the devices view only, never in the
  background. Multicast browsing every few seconds for a wizard nobody has
  open is noise on the network and on Thread.

## 9. The other direction: Chipi as a Matter bridge

Publishing Chipi items to Apple Home is the second role of the same plugin,
not a second plugin, and it replaces `specs/binding-homekit.md`. Apple Home
speaks Matter since iOS 16.1, and so do Google Home, Alexa and SmartThings.
A Matter **bridge** is exactly what the Hue bridge is in Apple Home: one
accessory that carries many bridged devices. Chipi becomes that accessory;
its items, KNX-bound or virtual, become the bridged devices. There is no
HAP, no SRP, no `avahi-publish` subprocess, and nothing to write that
cl-matter does not already have in its device side.

### 9.1 What cl-matter brings and what it needs

The device side has run on the real network: `make-basic-node`,
`make-device` with its fabric table, fail-safe and commissioning windows,
`start-device-advertising`, the onboarding payload with QR and manual codes,
persistence of fabrics, ACL and resumption records, and chip-tool has
commissioned, controlled, re-administered and restarted it.

Needed on top, all in cl-matter:

| Gap | Note |
|---|---|
| Aggregator device type 0x000E and Bridged Device Basic Information cluster 0x0039 | the two things that make a node a bridge; the latter is Basic Information minus a few attributes plus mandatory `Reachable` |
| `parts-list` for an endpoint that is not the root | today only the root lists parts; the aggregator endpoint must list the bridged ones |
| Endpoints added and removed on a live node | `add-endpoint` exists; removal, and notifying subscribers of the Descriptor's parts list, do not |
| Clusters bridged items need as a **server** | On/Off exists; Level Control, Boolean State, Temperature Measurement, Window Covering, Thermostat are the same definitions as section 2 asks for, used from the other side |
| Responder on SBCL | same multicast prerequisite as the controller side; deferred with it |

Scope for now: Apple Home only. Google Home, Alexa and SmartThings come
with the same code and are not tested until asked for.

Attestation is the catch, and it is a known one: Chipi presents the SDK's
test DAC (vendor `0xFFF1`, product `0x8000`). Apple Home then shows
"Uncertified Accessory" and offers "Add Anyway"; Google Home wants the
vendor registered in its developer console. Fine for a private house, not
for a product. Nothing to build now; one sentence in the README. Both this
and the hub requirement below are accepted for the time being.

Apple requires a home hub (HomePod or Apple TV) for any Matter accessory;
Chipi needs the hub reachable over IPv6 on the LAN, nothing else.

### 9.2 Shape

```
Apple Home / Google Home / Alexa          administrators of Chipi's bridge node
        |
  cl-matter device                        port 5540, commissionable then operational advertisement,
        |                                 fabrics in the storage directory
  bridge node                             endpoint 0 root, endpoint 1 Aggregator,
        |                                 endpoints 2.. one bridged device each
  matter-bridge binding                   one per item, or several naming one device
        |
  items (defitem, house definers)         the KNX-bound light, the virtual mode, the rule-driven flag
```

The two roles are the two actors of section 3.2: the bridge actor owns the
device's exchange manager on port 5540, the controller actor its own on an
ephemeral port, each polling itself on its pinned thread. The mDNS
responder runs the thread cl-matter gives it. One storage directory holds
`commissioner.state`, `nodes`, `bridge.state` and the endpoint-id map.

Chipi commissioned as a device and Chipi commissioning devices are two
nodes on two fabrics with no relation to each other. A Matter device that
Chipi controls (section 3) is *not* re-published through the bridge: Apple
Home pairs with it directly as another administrator, and a double hop
would only add latency. The bridge is for everything that is not Matter.

### 9.3 The binding

Direction is the mirror of section 3: a push from the item becomes a cluster
attribute change (`(setf attribute-value)`, which cl-matter reports to
subscribers by itself); a command from Apple Home runs the cluster's command
handler, which sets the item (`item:set-value` with `:push t`: the KNX
binding on the same item writes to the bus, and the bridge binding's own
push finds the attribute already equal and does nothing).

The device type comes from the item's house kind. `bind-item` gives the
binding its item, and the kind is in the item's tags, so the plain form is:

```lisp
(deflight light-kitchen "Kitchen"
  (knx-binding :ga "1/2/3" :dpt "1.001")
  (matter-bridge))                       ; -> On/Off Light, On/Off cluster, label "Kitchen"

(defcontact window-bath "Bath window"
  (knx-binding :ga "2/0/7" :dpt "1.019")
  (matter-bridge))                       ; -> Contact Sensor, Boolean State

(defitem 'garden-pump "Garden pump" 'boolean
  (matter-bridge :device-type :on-off-plug-in-unit))   ; no kind: say what it is
```

A device of several items names the endpoint they share:

```lisp
(defrollershutter blind-living "Blind living room"
  :move     ((knx-binding ...) (matter-bridge :device blind-living))
  :stop     ((knx-binding ...) (matter-bridge :device blind-living))
  :position ((knx-binding ...) (matter-bridge :device blind-living)))
;; -> one Window Covering endpoint: UpOrOpen / DownOrClose from the move item,
;;    StopMotion from the stop item, CurrentPositionLiftPercent100ths and
;;    GoToLiftPercentage from the position item -- each by its house kind

(defheating heating-bath "Bath heating"
  :temperature ((knx-binding ...) (matter-bridge :device heating-bath))
  :setpoint    ((knx-binding ...) (matter-bridge :device heating-bath))
  :valve       ((knx-binding ...)))      ; stays at home: Matter's Thermostat has no valve
;; -> one Thermostat endpoint: LocalTemperature, OccupiedHeatingSetpoint
```

House kind to bridged device, the mirror of section 5:

| House kind(s) | Bridged device type | Clusters served |
|---|---|---|
| `:light` | On/Off Light 0x0100 | On/Off |
| `:light` + a `:level` item on the same `:device` | Dimmable Light 0x0101 | On/Off, Level Control |
| `:plug` | On/Off Plug-in Unit 0x010A | On/Off |
| `:contact` | Contact Sensor 0x0015 | Boolean State |
| `:temperature` alone | Temperature Sensor 0x0302 | Temperature Measurement (× 100) |
| `:temperature`, `:setpoint`, `:mode` on one `:device` | Thermostat 0x0301 | Thermostat |
| `:shutter-move`, `:shutter-stop`, `:shutter-position`, `:shutter-slat` on one `:device` | Window Covering 0x0202 | Window Covering |
| `:pulse` | On/Off Plug-in Unit, released after the pulse | On/Off; Apple shows a switch that springs back |
| none | required `:device-type` | by the type |

Keys of `matter-bridge`: `:device` (a symbol; the endpoint several items
share), `:device-type` (overrides the kind), `:label` (the bridged device's
NodeLabel, default the item's label or the device's), `:call-push-p` (as
always; `nil` publishes a read-only value).

Every bridged endpoint carries Bridged Device Basic Information with
`NodeLabel` (the label), `Reachable` (true; false when a KNX binding on the
item reports its tunnel down, through the `binding-reachable-p` generic of
section 4.4) and `UniqueID` (a hash of the item or device name). Apple keeps
room, name and automations per bridged device by endpoint id, so **endpoint
ids must be stable across restarts**: a map from item or device name to
endpoint id lives in the storage directory, new names get the next free id,
and an id whose item vanished is kept reserved for a while rather than
reused, so a removed and re-added light stays the same light.

### 9.4 Pairing Chipi into Apple Home

```lisp
(matter-init :storage "~/.chipi/matter/"
             :bridge (:name "Chipi" :passcode 20202021 :discriminator 3840
                      :port 5540 :interface "en0"))
```

At start: the bridge node is built from the bindings, the device restored
from `bridge.state`, the operational advertisement starts for every fabric
it belongs to. A bridge with no fabric yet opens its basic commissioning
window and advertises as commissionable; one that has administrators does
not, and the window is opened on request. This is cl-matter's own
behaviour, nothing added.

The UI's *Controller* card of section 4.4 gets a sibling, **Share with
other apps**:

- the QR code and the manual pairing code (cl-matter encodes the payload
  string; the QR image needs a small pure-CL QR encoder, `cl-qrencode`, or
  the manual code alone, which Apple Home accepts typed in),
- "Open pairing window" with the minutes left (the window closes after 15
  minutes at most; Apple Home wants it open while adding),
- the administrators paired so far, by fabric label ("Apple Home",
  "Google Home" — what they write with `UpdateFabricLabel`), each with
  "Remove",
- the bridged devices as Apple sees them: endpoint, type, label, reachable.

The REPL gets the same: `(matter-bridge-pairing-code)`,
`(matter-bridge-open-window &key timeout)`, `(matter-bridge-admins)`,
`(matter-bridge-remove-admin fabric-index)`.

A change to the set of bridged items (a `defitem` added to the script and
reloaded) adds or removes endpoints on the live node; Apple Home picks up
the Descriptor change through its subscription and shows the new accessory
without re-pairing, as it does for a new Hue bulb.

### 9.5 Build order, bridge role

| Phase | Work | Done when |
|---|---|---|
| B0 | cl-matter: Aggregator and Bridged Device Basic Information, parts list per endpoint, endpoint removal with Descriptor change reports (done 2026-11-09, cl-matter d16ff19: `make-aggregator-endpoint`, `make-bridged-endpoint`, `remove-endpoint`, `examples/bridge-device.lisp`); the server-side clusters shared with phase 0 are still open | `test/data-model-test.lisp` and `test/interaction-test.lisp` cover it; chip-tool (now at `cl-matter/tools/chip-tool`) commissions the bridge, reads both lights' labels, toggles them and follows the PartsList of a light coming and going (2026-11-09) |
| B1 | `matter-bridge` binding for the single-item kinds (light, plug, contact, temperature), the endpoint-id map, the bridge actor next to the controller actor | a KNX light switched from the Home app and the Home app following the wall switch |
| B2 | composed devices: `:device`, shutter and heating | the living-room blind and the bath heating in Apple Home |
| B3 | UI card *Share with other apps*, QR image, admins; REPL functions | pairing from the wall tablet's QR |
| B4 | reachability from the KNX tunnel state; pulse kind | lights grey out in Apple Home while the gateway is down |

B0 and B1 can run in parallel with phases 1–3 of the controller role; they
touch different parts of cl-matter and share only the actor scaffolding and
the storage directory, which phase 1 builds.

### 9.6 What this means for the HomeKit spec

`specs/binding-homekit.md` planned a pure-CL HAP implementation: SRP-6a,
Ed25519 pairing, an HTTP server with encrypted framing, mDNS through a
subprocess. All of that is what Matter's device side already is in
cl-matter, with the same end result in the Home app. Two things HAP would
have given that the bridge does not: no "Uncertified Accessory" prompt (HAP
has no attestation), and HomeKit-only characteristics that Matter lacks,
which for our house kinds is nothing. Recommend: retire the HomeKit spec
in favour of section 9, and keep the file as the record of why.
