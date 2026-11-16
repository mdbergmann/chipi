### Chipi - Matter binding plugin

Chipi plays two Matter roles with this plugin:

- as a **bridge**, Chipi is a Matter device that Apple Home, Google Home or
  Alexa commission, and items -- KNX-bound or virtual -- are its bridged
  devices. This is what the plugin does today; it runs a KNX house in Apple
  Home (lights, window contacts, thermostats; since 2026-11-11, on AmiLisp).
- as a **controller**, Chipi commissions Matter devices into a fabric of its
  own, and they become items. Its core is there -- the fabric, the node
  registry, reading and switching a node -- and runs against an in-process
  device in the tests; discovery, the binding and subscriptions follow
  (`specs/binding-matter.md`, and *The controller, so far* below).

The protocol is [cl-matter](../../../cl-matter), a Matter implementation in
Common Lisp with a device and a controller side.

(Please see a [full example script](example-script.lisp).)

#### Setup

cl-matter is not on OCICL: make it visible to ASDF, from
`~/quicklisp/local-projects` or a sibling checkout, then load the plugin's
system definition from its folder:

```lisp
(push #p"<your>/cl-matter/" asdf:*central-registry*)
(asdf:load-asd "<your-chipi-clone>/bindings/matter/binding-matter.asd" :name "binding-matter")
(asdf:load-system :binding-matter)
```

The lines above should be within the `eval-when` of your item definition
script, as the example does.

Matter runs over IPv6 and finds devices over mDNS. Both are complete in
cl-matter's AmiLisp network backend; on SBCL the backend has IPv4 datagrams
only, so there the bridge is for the tests (IPv4 loopback, no mDNS), not for
Apple Home: **a bridge the apps can find means Chipi on AmiLisp**. The host
needs an IPv6 address on the interface the bridge advertises on (without
`:interface`, that is every interface with an address that is not
link-local), and UDP port 5540 and mDNS (UDP 5353) must pass. A Matter accessory in Apple Home also needs a
home hub, a HomePod or Apple TV, on the same network.

#### Usage

##### Initialization

In the body of `defconfig`:

```lisp
(defconfig "my-home"
  (matter-init :storage "~/.chipi/matter/"
               :bridge '(:name "Chipi" :passcode 20202021 :discriminator 3840
                         :interface "en0")))

;; ... every item, group, rule ...

(config-loaded)     ; the last form of the file: the bridge starts here
```

`matter-init` configures; the bridge starts at `hab:config-loaded`, when
every item is defined (see *When the bridge starts* below). Without that
form at the end of the configuration there is no bridge, and the log's last
word on it is `Matter bridge: configured, starts when the configuration is
loaded (hab:config-loaded)`.

`:storage` is the directory of the state files, a string (`~/` understood)
or a pathname, `matter/` in chipi's runtime directory by default; it is
created when missing. Two files live there:

| File | Holds | Losing it means |
|---|---|---|
| `bridge.state` | the fabrics the bridge was commissioned into with their keys, the ACL, the session resumption records, attributes the apps wrote; cl-matter rewrites it after every change | every app has to add the bridge again |
| `endpoints` | the endpoint id of every bridged device there ever was, by name | the devices get new ids: the apps take them for new ones, and rooms, names and automations are gone |

Both are plain printed Lisp data. `bridge.state` holds private keys: keep the
directory out of the repository and readable by the Chipi user only, and
back it up with the rest of the runtime data.

`:bridge` asks for the bridge; `t` takes every default. Its keys: `:name` (the
bridge's own name, "Chipi"), `:passcode` (20202021) and `:discriminator`
(3840) of its commissioning window, `:port` (5540), `:interface` (the one to
advertise on; default every interface with a routable IPv6 address) and
`:advertise` (`nil` leaves mDNS alone). `matter-init` without `:bridge` only
sets the storage.

20202021 is Matter's well-known test passcode. It only counts while the
pairing window is open, but then anyone on the network can add the bridge
with it: give a house its own, eight digits that are not a pattern
(11111111, 12345678 and the like are invalid), read from a file outside the
repository. The pairing codes are computed from passcode and discriminator,
so they are the same on every start.

A shutdown hook calls `matter-shutdown`, which stops the bridge; the state
files stay for the next start.

###### When the bridge starts

`matter-init` sits in the `defconfig` like `knx-init`, but the two are
different things: the KNX tunnel is a connection the items need while they
are defined, the bridge is a server that shows the items to the outside. So
`matter-init` only registers the bridge's start with `hab:add-to-startup`,
and `(config-loaded)`, the last form of the configuration, runs it.

The reason: a paired hub is back within a second of the bridge answering,
and what it then finds is the house as far as the file has loaded. A device
that is not there yet is one the hub removes, room and automations with it,
and adds as new a moment later. Started last, the bridge publishes every
device in one go, each once with all its items.

A bridge that cannot start (the port taken, no IPv6) is logged by
`config-loaded` as `Error on startup hook: ...` and does not end the load of
the file: the house runs without it. Loading the configuration again on a
running system leaves a running bridge as it is. `start-bridge` starts one
at once, for a test or the REPL.

Until binding-matter 0.4.0 `matter-init` started the bridge itself, and a
paired bridge had to be initialised by hand after the last item. A
configuration written that way has no bridge after the update until
`matter-init` is followed by `(config-loaded)`.

##### Pairing

The commissioning window is open from the start while no administrator has
commissioned the bridge, and the pairing codes are logged:

```
Matter bridge: manual pairing code 34970112332, QR code MT:-24J0AFN00KA0648G00
```

`(matter-bridge-pairing-code)` returns them again: the manual code as 11
digits, the QR code's text as second value. In the Home app, *Add Accessory*
> *More options* > *My Accessory isn't shown here* > *Enter code* takes the
manual code. The bridge presents the Matter SDK's test attestation
credentials, so Apple Home calls it an *Uncertified Accessory* and offers
*Add Anyway*; fine for a private house, not for a product.

The log then says `Matter bridge: commissioned into fabric ... as node ...
(index 1)`. Apple Home adds itself twice, as two fabrics: one of vendor id
`#x1349` labelled "Home" and one of vendor id `#x1384`, the iCloud
keychain's ("Apple Keychain" on the card). Both belong to the
one home, and `matter-bridge-admins` and the card list both.

Once commissioned, the window closes. To add the bridge to a second app --
another home, Google Home next to Apple Home -- open it again:

```lisp
(matter-bridge-open-window)              ; 15 minutes, the most Matter allows; :timeout seconds
(matter-bridge-window-open-p)            ; NIL, the seconds left, or T while it stays open
(matter-bridge-close-window)
(matter-bridge-admins)                   ; the apps that added the bridge, one plist per fabric
(matter-bridge-remove-admin 1)           ; by :fabric-index; the app loses the bridge
(matter-bridge-devices)                  ; the bridged devices as the apps see them
```

The window opened this way takes the bridge's own passcode, so the codes
above stay what they were.

The window opens by itself whenever the bridge has no administrator: at a
start that finds none, and when the last one is removed -- with
`matter-bridge-remove-admin`, or by removing the bridge in the app. It then
stays open until an app adds the bridge.

##### The card in chipi-ui: Share with other apps

With chipi-ui, the same comes as a card in the settings, for the wall tablet.
Load the system `binding-matter/ui` next to `chipi-ui`:

```lisp
(asdf:load-system :binding-matter/ui)
```

The settings view then shows *Share with other apps*, with "2 apps, 5
devices, pairing window open" as its line, and the view behind it
(`/settings/matter-bridge`) has three cards: the pairing code as a QR image
to scan and as the number to type, with the pairing window's state and a
button to open it for 15 minutes or close it; the apps that added the bridge,
each with a *Remove* that wants a second tap; and the bridged devices as the
apps see them, with type, endpoint and items, or why one is not published.
The device lock of the settings covers the card. The QR image is drawn by
the binding's own encoder (`qr-encode`, `qr-svg`), so no library and no
network is needed for it.

##### The binding

An item is published by giving it a `matter-bridge` binding next to whatever
binding connects it to the house:

```lisp
(deflight light-kitchen "Kitchen"
  (knx-binding :ga "1/2/3" :dpt "1.001")
  (matter-bridge))
```

The house kind the definer gives the item decides what the Home app shows:

| Definer | House kind | Bridged device | Cluster |
|---|---|---|---|
| `deflight` | `:light` | On/Off Light | On/Off |
| `defdimmablelight`, switch and brightness on one `:device` | `:light`, `:brightness` | Dimmable Light | On/Off, Level Control |
| `defplug` | `:plug` | On/Off Plug-in Unit | On/Off |
| `defpulse` | `:pulse` | On/Off Plug-in Unit that springs back | On/Off |
| `defcontact` | `:contact` | Contact Sensor | Boolean State (true while closed) |
| `defheating`'s `:temperature` alone | `:temperature` | Temperature Sensor | Temperature Measurement (hundredths of a degree) |
| `defrollershutter`, channels on one `:device` | `:shutter-move`, `:shutter-stop`, `:shutter-position`, `:shutter-slat` | Window Covering | Window Covering |
| `defheating`, temperature and setpoint on one `:device` | `:temperature`, `:setpoint` | Thermostat (heating only) | Thermostat |

An item without one of these kinds says what it is with `:device-type`, one
of `:on-off-light`, `:dimmable-light`, `:on-off-plug-in-unit`, `:contact-sensor`,
`:temperature-sensor`, `:window-covering`, `:thermostat`; an item with
neither is logged and not published. `:label` replaces the item's label as
the name the app shows. The other keys go to the base binding;
`:call-push-p` is `t` by default.

```lisp
(defitem 'garden-pump "Garden pump" 'boolean
  (matter-bridge :device-type :on-off-plug-in-unit :label "Pump"))
```

Both directions work. The bridge mirrors the item, whoever changed it: it
follows the item's change event (`item:item-changed-event`), so a value
another binding brought in -- a KNX telegram from the wall switch, the
answer to a read, which are set with `:push nil` and pushed to no binding --
reaches the device's attribute like one set from a rule or the UI.
Administrators that subscribed (the Home app does) hear of it at once. A
command from the app -- On, Off, Toggle -- sets the item with push, so a KNX
binding on the same item writes it to the bus, and the bridge itself finds
the attribute already equal and does nothing. The binding's own push is
used for one thing only: a shutter's move and stop commands, which may
repeat the value the item holds (`house:send-command`, `:push :always`) and
then raise no change event.

A `defpulse` -- a door opener, a bell -- is a switch that springs back:
On in the app presses the button (`house:press`, with the duration the
`defpulse` gave), and the release switches the app's tile off again. An Off
in between does nothing, the release comes by itself. A press at home shows
in the app the same way. Matter's own button, the Generic Switch, only
reports presses and cannot be pressed from an app.

###### Devices of several items

A dimmable light, a shutter or a heating is several items in the house and one device in the
app. The bindings of its items name the device they share with `:device`,
a symbol taken as written, like the name in a definer:

```lisp
(defdimmablelight light-living "Living room"
  :switch     ((knx-binding :ga '(:read "1/1/2" :write "1/1/1") :dpt "1.001")
               (matter-bridge :device light-living))
  :brightness ((knx-binding :ga '(:read "1/1/5" :write "1/1/4") :dpt "5.001")
               (matter-bridge :device light-living)))

(defrollershutter blind-living "Blind living room"
  :move     ((knx-binding :ga "4/0/31" :dpt "1.008") (matter-bridge :device blind-living))
  :stop     ((knx-binding :ga "4/0/30" :dpt "1.007") (matter-bridge :device blind-living))
  :position ((knx-binding :ga '(:read "4/0/33" :write "4/0/32") :dpt "5.001")
             (matter-bridge :device blind-living)))

(defheating bath "Bath"
  :temperature ((knx-binding :ga "0/4/17" :dpt "9.001") (matter-bridge :device bath))
  :setpoint    ((knx-binding :ga '(:read "0/5/19" :write "0/5/18") :dpt "9.001")
                (matter-bridge :device bath))
  :valve       ((knx-binding :ga "0/2/6" :dpt "5.001")))   ; stays at home
```

The house kinds of the items decide the device: a light with a brightness
is a **Dimmable Light**, shutter channels make a
**Window Covering**, a setpoint (with or without a temperature) a
**Thermostat**. The app's name for the device is what the items' labels
begin with in common ("Blind living room", "Bath"), or the `:label` one of
the bindings gives.

A Dimmable Light has On/Off from the switch item and its level from the
brightness item: 0 to 100 % in the house, the levels 1 to 254 in Matter,
which has no level 0 -- off is the switch. A brightness set in the app
switches the switch item on first when the light is off and then sets the
brightness item, as a command (`:push :always`), so the dimmer hears it
also when the item holds that value already; the lowest level sets 1 % and
then switches off. The app's On and Off set the switch item only, and the
brightness the dimmer comes on with reaches the app when the brightness
item reports it. Transition times are ignored, the dimmer has its own. The
device needs the switch item: a brightness item alone is not published.

A Window Covering has lift, and tilt when there is a slat item; it is
position aware when there is a position item. Open, Close and Stop in the
app send the shutter's move and stop commands through the move and stop
items (a shutter without a move item is driven through its position item);
a position the app asks for sets the position item. The position item
reports the current position, 100 % closed on both sides. The app shows
"opening" or "closing" while the current position is on its way to a target
-- the one the app or a move command set -- and the target follows the
position otherwise, so a shutter stopped at the wall shows where it stopped.

A Thermostat has the local temperature from the temperature item and the
heating setpoint from the setpoint item, 5 to 35 degrees; a setpoint
written or stepped in the app sets the setpoint item with push, so the KNX
binding writes it to the bus. The valve stays at home: Matter's Thermostat
has none.

The system mode -- Off and Heat in the app -- is the mode item of the
`defheating`, if it has one in the device:

```lisp
(defheating bath "Bath"
  :setpoint ((knx-binding ...) (matter-bridge :device bath))
  :mode     ((knx-binding :ga "0/6/2" :dpt "20.102")
             (matter-bridge :device bath)))          ; :modes '(:off 4 :heat 1) by default
```

Off sets the item to its `:off` number and Heat to its `:heat` number, with
push; the item reads as Off while it holds the `:off` number and as Heat
with any other, so standby and night still show as heating. The defaults
are KNX's HVAC mode (DPT 20.102): 4, building protection, and 1, comfort;
`:modes` on the mode item's `matter-bridge` gives others. A Thermostat
without a mode item heats: the app's Off is refused and the tile stays on
Heat.

A device is published afresh as its items join and leave it, so reloading a
definer keeps the device whole, and a device whose items make no sense
together -- two of a kind, kinds of different devices, device types that
disagree -- is logged and left out until they do.

Each bridged device carries Bridged Device Basic Information with its label,
`Reachable` (see below) and a `UniqueID` derived from the device's name. The app
keeps room, name and automations of a device by its endpoint id, so endpoint
ids are stable: a name gets the id it had before, a new name the next one,
and an id is never given to another name, even after its item is gone.
Redefining an item (reloading the script) withdraws and republishes its
endpoint under the same id; the app follows the Descriptor's PartsList
without re-pairing.

###### Reachable

A bridged device is reachable while every *other* binding of its items says
so (`binding:binding-reachable-p`): a KNX binding answers with the state of
its tunnel, a function binding always with true, and a device of items
without another binding is always reachable -- Chipi is where it lives. The
bridge asks when it publishes a device and again on every
`binding:reachability-changed-event`, which binding-knx publishes when the
tunnel comes up, is lost and is back. While the gateway is down the apps
show the KNX devices as not responding instead of showing the last value
as current and taking commands that go nowhere; the log says
`Matter bridge: <device> is unreachable` and `... is reachable again`. A
binding of your own takes part by answering the generic function and
publishing the event (see the main README, *Writing your own*).

##### In a house config

A house usually wraps the definers in macros of its own that know its group
addresses. Sharing a device is then one option of the wrapper, on by default
or off, whatever suits the kind:

```lisp
(defmacro deflight (name label &key ga (matter t))
  "A switched light on GA. Shared over the Matter bridge unless `:matter nil'."
  `(house:deflight ,name ,label
     (knx-binding :ga ,ga :dpt "1.001")
     ,@(when matter '((binding-matter:matter-bridge)))))

(deflight light-kitchen "Kitchen" :ga "0/0/7")
(deflight light-cellar "Cellar" :ga "0/0/9" :matter nil)   ; stays at home
```

A shutter's wrapper puts `(matter-bridge :device ,name)` on each channel; a
heating's adds `:label ,label` to one of them when the items' labels begin
with nothing in common ("Temperature Bath", "Setpoint Bath").

Choose what to share with the app in mind: every bridged device is a tile
there, and one that is renamed in the config (a new item name, or a new
`:device`) is a new device to the app. The label may change freely.

##### Restarts

Stopping Chipi withdraws the mDNS record and closes the socket; nothing has
to be done in the apps, and nothing is paired again afterwards. What the
apps show in between:

- While the bridge is down, and until the hub is back, the app shows what
  it last heard and its commands wait or fail.
- A hub notices that the bridge was restarted when a report it was promised
  does not come. The bridge grants subscribers a report at least every
  `*bridge-max-report-interval*` seconds, 60, whatever longer interval they
  ask for (Apple Home asks for 600: before this, the tiles were stale for
  ten minutes after a restart). So within about a minute of the bridge
  being back the hub opens a new session (resumed from `bridge.state`, no
  new pairing) and subscribes again; the log shows both. The variable is
  read when the bridge starts.
- Reloading a definer on the running system withdraws and republishes the
  device under its endpoint id; a subscribed hub is told the whole endpoint
  and keeps room and name.

##### The log

The bridge logs under the category `chipi.binding.matter`, at info: who
pairs, every session and subscription that comes and goes, every command
from an app. A config that turns Chipi down to warnings after its start
should leave this one up -- it is how one sees that the hub is there:

```lisp
(log:config '(chipi) :warn)
(log:config '(chipi binding matter) :info)
```

A start, in this order:

```
Matter: state files in /home/chipi/my-home/runtime/matter/
Matter bridge: configured, starts when the configuration is loaded (hab:config-loaded)
...
Configuration loaded, executing startup hooks (1)...
Matter bridge: published LIGHT-KITCHEN as ON-OFF-LIGHT on endpoint 2 (LIGHT-KITCHEN)
Matter bridge: published BATH as THERMOSTAT on endpoint 5 (BATH-TEMP, BATH-SETPOINT)
Matter actor matter-bridge: polling
Matter bridge: advertising Chipi on fe80::..., fd12:...
Matter bridge Chipi on [::]:5540, 2 fabrics, 14 bridged devices
Matter bridge: manual pairing code 34970112332, QR code MT:-24J0AFN00KA0648G00
Matter bridge: session opened by node 00000000A1B2C3D4 of fabric 0000000012345678 at fd12:...
Matter bridge: node 00000000A1B2C3D4 of fabric 0000000012345678 at fd12:... subscribed to 3 paths, a report at least every 60 s
```

Endpoint 1 is the aggregator, the devices start at 2. In operation:

| Line | Means |
|---|---|
| `<device> switched on`, `down <device>`, `stop <device>`, `<device> to 50 %`, `<device> slats to 30 %`, `<device> setpoint 21.5` | a command from an app, about to be set on the item |
| `<device> system mode 0, mode 4` | the app's Off (0) or Heat (4), and the number about to be set on the mode item |
| `identify <device> for N seconds` | the app's "identify" while adding a device |
| `<device> is unreachable` / `is reachable again` | another binding of its items lost or regained its connection |
| `session opened by ...` / `session closed: ...` | an administrator's CASE session |
| `... subscribed to N paths, a report at least every 60 s` / `subscription of ... ended: <reason>` | a hub's subscription; after a restart of the bridge a new one follows within about a minute |
| `pairing started by a commissioner at ...`, `commissioned into fabric ...`, `fabric ... removed` | an app adding or removing the bridge |
| `pairing window open for 900 seconds` / `pairing window closed` | the window opened or closed from the card or the REPL |
| `published ... on endpoint N (...)` / `withdrew endpoint N (...)` | a device added to or taken off the live node |

And the ones to stop at:

| Line | Means |
|---|---|
| `not advertised over mDNS (...)` | the apps cannot find the bridge: the interface has no usable address or the responder failed (`:interface` wrong?); always so on SBCL |
| no `Matter bridge Chipi on ...` line after `configured, starts when ...` | the configuration does not end with `(config-loaded)` |
| `Error on startup hook: ...` (error) | the bridge could not start: the port is taken, the socket could not be opened, a key of `:bridge` is wrong |
| `cannot publish ...` | a device's items make no sense together, or an item has neither a house kind the bridge knows nor a `:device-type`; `(matter-bridge-devices)` has the reason under `:problem`, and so has the card |
| `pairing attempt failed: ...` / `session attempt failed: ...` (warn) | a handshake a peer began and did not finish: a wrong code, for one; an app that still holds the bridge after `bridge.state` was deleted would be another -- remove the bridge there and add it again |
| `Matter actor matter-bridge: error in poll: ...` (warn) | one datagram cl-matter could not handle; the bridge goes on |
| `<device> has no stop item, the stop is dropped` (warn) | a Window Covering without a stop item got a Stop |

From the shell, without a phone: `dns-sd -Z _matterc._udp local.` (macOS;
`avahi-browse -r _matterc._udp` on Linux) shows the bridge while its pairing
window is open, one SRV on port 5540 with `D=3840`, `CM=1` and `DN=Chipi` in
the TXT; `_matter._tcp` shows one instance per fabric once it is paired.

##### What is not there yet

- **Coloured lights**: only the device types of the table are published.
- **Certification.** The bridge presents the SDK's test attestation
  credentials and the test vendor id #xFFF1; "Uncertified Accessory" stays.
- **OTA.** Apple Home writes to an OTA requestor cluster the bridge does not
  have and takes the refusal; harmless.
- **SBCL.** No IPv6 and no mDNS in cl-matter's SBCL backend: tests only.
- **The controller role** -- Matter devices as items -- has its core only;
  see the next section.

##### The controller, so far

Phase 1 of `specs/binding-matter.md`: what the binding and the pairing
wizard will stand on. Nothing here finds a device on the network yet.

```lisp
(defconfig "my-home"
  (matter-init :storage "~/.chipi/matter/"
               :controller '(:fabric-id #xC1 :attestation :ignore)))
```

`:controller` -- `t` or a plist -- starts the controller at once, in the
`defconfig`, unlike the bridge: the items defined after it pull from it.
Both roles can run in one Chipi; they share the storage directory and
nothing else. The keys:

| Key | Default | |
|---|---|---|
| `:fabric-id` | 1 | the id of Chipi's fabric, used when the fabric is made: the first start. A fabric on file is the one that stays, a different id here is logged and ignored |
| `:vendor-id` | `#xFFF1` | the test vendor, fine for a private fabric |
| `:attestation` | `:ignore` | how a device is checked when it is commissioned: `:ignore` accepts any device that holds the key of its attestation certificate, `:verify` checks the chain against cl-matter's trust store, which has the SDK's test authorities only |
| `:label` | `"Chipi"` | the name the nodes list Chipi's fabric under for their other administrators (`UpdateFabricLabel` after commissioning) |

Two files in the storage directory: `commissioner.state` -- the fabric with
its root key, the node ids handed out, the resumption records; whoever has
this file administers every node of the fabric, and without it the nodes
have to be reset and commissioned again -- and `nodes`, the registry: per
node its id, the label it was given, what its Basic Information and the
Descriptor of each endpoint said (vendor, product, serial number, device
types, server clusters), the address it was reached at.

```lisp
(matter-nodes)                                  ; => plists, oldest first
(matter-read "lamp" 1 :on-off :on-off)          ; => a future of T or NIL
(matter-invoke "lamp" 1 :on-off :toggle)        ; => a future of T
(matter-invoke "lamp" 1 :level-control :move-to-level
               '(:level 128 :transition-time 0 :options-mask 0 :options-override 0))
```

A node is named by its label or its node id, cluster, attribute and command
by the names cl-matter's definitions have for them (as keywords) or by
their ids -- a cluster cl-matter does not define is addressed by ids, and
its values come as TLV elements. Both functions return a Sento future at
once; the answer is a datagram a later turn of the controller actor
receives. A future fails with the condition that says why not (no such
node, no session, no answer, a status from the node): `future:frecover`
handles it, `future:error-p` tells. That is the shape a binding wants -- a
pull hands the future to chipi, a push drops it:

```lisp
(defitem 'lamp "Lamp" 'boolean
  (binding :pull (lambda ()
                   ;; what the lamp says is not sent back to it
                   (values (matter-read "lamp" 1 :on-off :on-off) '(:push nil)))
           :transform (lambda (on) (if on 'item:true 'item:false))
           :push (lambda (value)
                   (matter-invoke "lamp" 1 :on-off (if (eq value 'item:true) :on :off)))
           :initial-delay 2 :delay 60
           :call-push-p t))
```

The `matter-binding` form that does this by itself, with subscriptions in
place of the polling, is the next phase.

One CASE session per node, opened by the first request and kept; requests
that arrive while it is being opened wait for it together. A request that
gets no answer costs the session, and the next one opens a new one. The
node is reached at the address it was commissioned at: no operational
discovery yet, so a node whose address changed is lost until then.

Commissioning is there as the internal `%commission-at` -- address, port,
passcode and label given -- which the integration test uses: PASE,
attestation, the NOC, CommissioningComplete, then the read of what the node
is and the fabric label. `matter-commission`, which finds the device from
its pairing code, comes with discovery (phase 3).

##### How it runs

cl-matter is single-threaded and polled. Each role owns one Sento actor
(`matter-bridge`, `matter-controller`) of the kind described here for the
bridge; the controller's requests are `matter-actor-future`s, resolved by
the actor when the answer came. The bridge owns one Sento actor on
a thread of its own (`make-matter-actor`, pinned) that polls the exchange
manager, a turn of at most 50 ms; every change to the data model and every
command handler runs in that actor. A second, ordinary actor
(`matter-bridge-items`) subscribes to the item change and reachability
events and hands each on as a message to the first, and a command from the
app ends in `item:set-value`, which never blocks. The functions for the
REPL and the card (`matter-bridge-admins` and the others) are calls into
the bridge actor that wait up to 10 seconds for its answer. `hab:shutdown`
stops both actors; the bridge actor withdraws the mDNS advertisement and
closes the socket on its way out.

The files: `matter-bridge.lisp` (the bridge, the binding, the functions
above), `matter-actors.lisp` (the polling actor), `matter-convert.lisp`
(house kinds to device types, item values to attribute values),
`qr-code.lisp` (the QR encoder), `matter-ui.lisp` (the settings card, system
`binding-matter/ui`), `matter-registry.lisp` (the controller's node
registry), `matter-controller.lisp` (the controller: fabric, sessions,
`matter-read`, `matter-invoke`), `binding-matter.lisp` (`matter-init`,
`matter-shutdown`).

#### Tests

```lisp
(push #p"<your>/cl-matter/" asdf:*central-registry*)
(asdf:load-asd "<your-chipi-clone>/bindings/matter/binding-matter.asd" :name "binding-matter")
(asdf:test-system :binding-matter)
```

runs the unit tests (conversions, endpoint ids, the QR encoder against
node-qrcode's symbols, the bridge on loopback without a network) and the
integration test, in which cl-matter's own commissioner commissions the
bridge over loopback UDP, switches a light, reads the item's change back and
writes a heating's setpoint. The controller has the same two: unit tests
(registry, futures, start and stop) and an integration test in which it
commissions an in-process cl-matter lamp over loopback UDP, and a `defitem`
reads and switches it. `(asdf:test-system :binding-matter/ui-tests)`
renders the settings card into a stubbed CLOG connection. All run on SBCL
and on AmiLisp. From the shell, `scripts/review/run-tests.sh matter` runs
both suites on SBCL and exits non-zero when a check fails (`CL_MATTER_DIR`
names the cl-matter checkout when it is not the sibling directory); the
commit hook runs the core suite only, so run this one after changes to the
plugin, to chipi-ui's settings or to cl-matter's data model.

The tests start the bridge with `:port 0 :family :ipv4 :address "127.0.0.1"
:advertise nil`, keys of `start-bridge` that exist for them.
