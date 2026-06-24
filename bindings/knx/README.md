
### Chipi - KNX binding plugin documentation

The Chipi KNX binding allows items to interact with the KNX bus system and group-addresses.

For example:

- an item definition represents a temperatur sensor in the house
- it will use the `knx-binding` to interact with the temperatur-sensor uni- or bidirectional

The interaction with a temperatur sensor is most likely only unidirectional where the item state (though the knx-binding) is updated asynchronously from the KNX bus where the temperature sensor posts it's temperatur updates.

In case of (i.e.) a light you'd want the item state be updated from the bus but also switch the light on/off through changing the item value (bidirectional, i.e. via API or UI).

(Please see a [full example script](example-script.lisp))

#### Setup

In order to use the KNX binding it must be loaded via:

```lisp
(asdf:load-asd "<your-chipi-clone>/bindings/knx/binding-knx.asd" :name "binding-knx")
```

and then:

```lisp
(asdf:load-system :binding-knx)
```

As of right now it is required to clone the Chipi project to some folder in order have a predictable folder path to load the asd and system.

The lines above should be within the `eval-when` of your item definition script:

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
-> somewhere here
)
```

#### Usage

##### Initialization

In the items script the body of `defconfig` can now contain a `knx-init` defined as part of the plugin.
I.e.:

```lisp
(defconfig
  (knx-init :gw-host "192.168.1.23"))
```

Optionally specify a port via `:gw-port` which defaults to 3671 (the default KNXNet/IP port).

(see also "binding-knx.lisp" for more info, or also the "binding-knx-integtest.lisp")

To shutdown the KNX environment it is possible to call `knx-destroy` (i.e. for testing stuff).

##### The binding

To connect items to KNX bus GAs (group-addresses) you do like this:

```lisp
(defitem 'my-light-foo "Light Foo" 'boolean
  (knx-binding :ga "1/2/3" ;; or: '(:read "1/2/3" :write "1/2/4")
               :dpt "1.001"
               :initial-delay 2
               :call-push-p t))
```

This creates a new item and attaches a `knx-binding`.

`:ga` specifies the group-address as string if the same ga should be used for read and write. Sometimes actors define separate connections for write and read (status) in which case `:ga` allows to specify separate group-addresses, i.e. this is also allowed: `'(:read "1/2/3" :write "1/2/4")`.  
`:dpt` specifies the type of DPT. This is required to make the right value conversions since the DPT type of the GA is not known in the KNXNet/IP protocol.  
`:initial-delay 2` specifies that after 2 seconds the value of the light is requested from the bus to initialize the item with.  
`:call-push-p` set to `T` allows to also post item value updates to send to the KNX bus. In other words, this allows to set the state of the light via the bus when the item value is changed via `item-set-value`.

It'a also possible to use `:delay` to periodically update the item value from the bus but this is actually not needed as the binding listens on bus changes for the specified GA and updates the item value asynchronously.  
If some action should be performed upon a certain item value change it is possible to specify a Chipi rule via `defrule`.

##### Reliable writes

Every write goes out via `knxc:write-value` with a transport-level retry, so a write that comes back with a negative `L_Data.con` or an ACK/response timeout is resent instead of silently dropped (see `*write-retries*` / `*write-retry-backoff*`).

That only guarantees the telegram reached the bus — not that the device actually switched. For that, enable the optional **read-back verify** policy: after a push the actuator status is read back from the read GA and, on mismatch, the value is re-pushed; if it still cannot be confirmed, `:verify-on-fail` is called (e.g. to raise an alarm). Works for any DPT.

```lisp
(defitem 'my-light-foo "Light Foo" 'boolean
  (knx-binding :ga '(:read "1/2/3" :write "1/2/4")  ;; read GA must have its read flag set
               :dpt "1.001"
               :call-push-p t
               :verify t
               :verify-on-fail (lambda (info)
                                 (log:warn "switch not confirmed: ~a" info))))
```

Verify keys (all optional):

- `:verify` — enable read-back verification.
- `:verify-delay` — seconds to wait after a push before reading back (default `*default-verify-delay*`).
- `:verify-retries` — number of re-push attempts on mismatch (default `*default-verify-retries*`).
- `:verify-tolerance` — allowed absolute difference for numeric DPTs (floats); ignored for booleans.
- `:verify-compare` — optional `(desired actual)` predicate overriding the default DPT-aware comparison.
- `:verify-on-fail` — optional `(info-plist)` callback invoked when verification is exhausted or a write fails permanently. The plist holds `:binding`, `:write-ga`, `:desired` and `:actual`.

Verify never blocks the shared timer thread or the item actors: the delay is honoured by the timer, and the read-back runs on a small dedicated worker pool whose size is set via `knx-init :verify-workers` (default 2).
