### Chipi - Matter binding plugin

Chipi plays two Matter roles with this plugin:

- as a **bridge**, Chipi is a Matter device that Apple Home, Google Home or
  Alexa commission, and items -- KNX-bound or virtual -- are its bridged
  devices. This is what the plugin does today.
- as a **controller**, Chipi commissions Matter devices into a fabric of its
  own, and they become items. This follows (`specs/binding-matter.md`).

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
Apple Home. A Matter accessory in Apple Home also needs a home hub, a HomePod
or Apple TV, on the same network.

#### Usage

##### Initialization

In the body of `defconfig`:

```lisp
(defconfig "my-home"
  (matter-init :storage "~/.chipi/matter/"
               :bridge '(:name "Chipi" :passcode 20202021 :discriminator 3840
                         :interface "en0")))
```

`:storage` is the directory of the state files, `matter/` in chipi's runtime
directory by default: `bridge.state` holds the fabrics the bridge was
commissioned into, with their keys, the ACL and the session resumption
records (cl-matter rewrites it after every change), and `endpoints` holds the
endpoint ids of the bridged devices.

`:bridge` starts the bridge. Its keys: `:name` (the bridge's own name,
"Chipi"), `:passcode` (20202021) and `:discriminator` (3840) of its
commissioning window, `:port` (5540), `:interface` (the one to advertise on;
default every interface with a routable IPv6 address) and `:advertise` (`nil`
leaves mDNS alone). `matter-init` without `:bridge` only sets the storage.

A shutdown hook calls `matter-shutdown`, which stops the bridge; the state
files stay for the next start.

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

Once commissioned, the window closes. Opening it again for a second
administrator, and the list of administrators, come with the UI's *Share with
other apps* card (phase B3 of the plan).

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
| `defplug` | `:plug` | On/Off Plug-in Unit | On/Off |
| `defcontact` | `:contact` | Contact Sensor | Boolean State (true while closed) |
| `defheating`'s `:temperature` | `:temperature` | Temperature Sensor | Temperature Measurement (hundredths of a degree) |

An item without one of these kinds says what it is with `:device-type`, one
of `:on-off-light`, `:on-off-plug-in-unit`, `:contact-sensor`,
`:temperature-sensor`; an item with neither is logged and not published.
`:label` replaces the item's label as the name the app shows. The other keys
go to the base binding; `:call-push-p` is `t` by default.

```lisp
(defitem 'garden-pump "Garden pump" 'boolean
  (matter-bridge :device-type :on-off-plug-in-unit :label "Pump"))
```

Both directions work: the item's value goes to the device's attribute on
every change, and administrators that subscribed (the Home app does) hear of
it at once. A command from the app -- On, Off, Toggle -- sets the item with
push, so a KNX binding on the same item writes it to the bus, and the bridge
binding itself finds the attribute already equal and does nothing.

Each bridged device carries Bridged Device Basic Information with its label,
`Reachable` (true) and a `UniqueID` derived from the item's name. The app
keeps room, name and automations of a device by its endpoint id, so endpoint
ids are stable: a name gets the id it had before, a new name the next one,
and an id is never given to another name, even after its item is gone.
Redefining an item (reloading the script) withdraws and republishes its
endpoint under the same id; the app follows the Descriptor's PartsList
without re-pairing.

Composed devices -- a rollershutter, a heating with setpoint -- and
reachability from the KNX tunnel state are the next phases of the plan.

##### How it runs

cl-matter is single-threaded and polled. The bridge owns one Sento actor on
a thread of its own that polls the exchange manager; every change to the
data model and every command handler runs in that actor. A push from an item
is a message to it, and a command from the app ends in `item:set-value`,
which never blocks. `hab:shutdown` stops the actor, which withdraws the mDNS
advertisement and closes the socket.

#### Tests

```lisp
(push #p"<your>/cl-matter/" asdf:*central-registry*)
(asdf:load-asd "<your-chipi-clone>/bindings/matter/binding-matter.asd" :name "binding-matter")
(asdf:test-system :binding-matter)
```

runs the unit tests (conversions, endpoint ids, the bridge on loopback
without a network) and the integration test, in which cl-matter's own
commissioner commissions the bridge over loopback UDP, switches a light and
reads the item's change back. Both run on SBCL and on AmiLisp.
