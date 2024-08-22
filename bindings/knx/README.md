
### Chipi - KNX binding plugin documentation

The Chipi KNX binding allows items to interact with the KNX BUS and group-addresses.

For example:

- an item definition represents a temperatur sensor in the house
- it will use the `knx-binding` to interact with the temperatur-sensor uni- or bidirectional

The interaction with a temperatur sensor is most likely only unidirectional where the item state (though the knx-binding) is updated asynchronously from the KNX bus where the temperature sensor posts it's temperatur updates.

In case of (i.e.) a light you's want the item state be updated from the bus but also switch the light on/off through changing the item value (i.e. via API or UI).

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
  (knx-binding :ga "1/2/3"
               :dpt "1.001"
               :initial-delay 2
               :call-push-p t))
```

This creates a new item and attaches a `knx-binding`. `:ga` specifies the group-address as string.
`:dpt` specifies the type of DPT. This is required to make the right value conversions since the DPT type of the GA is not known in the KNXNet/IP protocol. `:initial-delay 2` specifies that after 2 seconds the value of the light is requested from the bus to initialize the item with. `:call-push-p` set to `T` allows to also post item value updates to send to the KNX bus. In other words, this allows to set the state of the light via the bus when the item value is changed via `item-set-value`.

It'a also possible to use `:delay` to periodically update the item value from the bus but this is actually not needed as the binding listens on bus changes for the specified GA and updates the item value asynchronously.  
If some action should be performed upon a certain item value change it is possible to specify a Chipi rule via `defrule`.
