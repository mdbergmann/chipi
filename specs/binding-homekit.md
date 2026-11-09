# HomeKit Binding Plugin — Specification

> **Superseded (2026-11-09).** Chipi items reach Apple Home through the
> Matter bridge role of the Matter binding, see `binding-matter.md`
> section 9. This HAP design is not going to be built; the file stays as the
> record of what was considered and why Matter won (section 9.6 there).

## Overview

A pure Common Lisp HomeKit Accessory Protocol (HAP) binding for Chipi, enabling Apple Home app and Siri control of Chipi-managed devices. The binding operates as a **HAP bridge** — a single HomeKit device that exposes multiple Chipi items as HomeKit accessories.

**Target platforms**: x86-64 Linux/macOS, ARM64 Linux (Raspberry Pi). Pure CL approach chosen for portability — no CFFI/C dependencies beyond the Lisp runtime.

## Architecture

### Role Difference from KNX

Unlike the KNX binding (client connecting to an external gateway), the HomeKit binding is a **server** — Apple devices connect to it. There is no pull/push cycle. Instead:

- Chipi advertises itself on the local network via mDNS
- iOS/macOS devices discover, pair with, and connect to Chipi
- Connected controllers read/write characteristics over encrypted HTTP
- Chipi pushes event notifications when item states change

### System Diagram

```
Apple Home App / Siri
        |
   HAP Protocol (mDNS discovery, encrypted HTTP, TLV8 pairing)
        |
   HAP Server (pure CL: usocket, Ironclad crypto)
        |
   HomeKit Binding (item <-> accessory mapping, state sync)
        |
   Chipi Item Registry (items, itemgroups, actor messaging)
        |
   Other Bindings (KNX, MQTT, etc.)
```

### Data Flow

**Inbound (Apple Home -> Chipi):**
1. Controller sends characteristic write via encrypted HTTP PUT to `/characteristics`
2. HAP server decrypts, parses JSON payload `{"characteristics": [{"aid": 1, "iid": 10, "value": true}]}`
3. HomeKit binding maps AID.IID to Chipi item
4. Calls `item:set-value` on the target item (with `:push nil` to avoid echo)
5. Item state propagates to other bindings (e.g. KNX writes to bus)

**Outbound (Chipi -> Apple Home):**
1. Item state changes (from KNX, rules, API, etc.)
2. `push-to-bindings` triggers HomeKit binding's push function
3. Binding maps item to AID.IID, formats HAP event notification
4. HAP server sends `EVENT/1.0 200 OK` to all subscribed controllers

**Read requests:**
1. Controller sends GET `/characteristics?id=AID.IID`
2. HAP server reads current item state, returns JSON response

## HAP Protocol Implementation

### Discovery — mDNS/Bonjour

Advertise a `_hap._tcp.local` service with TXT records:

| Key  | Description                              | Example              |
|------|------------------------------------------|----------------------|
| `c#` | Configuration number (increment on change) | `1`                |
| `ff` | Feature flags                            | `0`                  |
| `id` | Device ID (MAC-like XX:XX:XX:XX:XX:XX)   | `A1:B2:C3:D4:E5:F6` |
| `md` | Model name                               | `Chipi Bridge`       |
| `pv` | Protocol version                         | `1.1`                |
| `s#` | State number                             | `1`                  |
| `sf` | Status flag: 1 = not paired, 0 = paired  | `1`                 |
| `ci` | Category identifier (2 = Bridge)         | `2`                  |

**Implementation**: Shell out to platform mDNS tools:
- **Linux (Raspberry Pi)**: `avahi-publish-service` (pre-installed on Raspbian)
- **macOS**: `dns-sd` command

This avoids CFFI entirely. The subprocess is managed by the binding lifecycle (started on init, killed on shutdown).

### Pairing — Pair Setup (One-Time, PIN-Based)

6-step TLV8 exchange over HTTP POST to `/pair-setup`:

| Step | Direction          | Content                                                |
|------|--------------------|--------------------------------------------------------|
| M1   | Controller -> Acc  | State=1, Method=PairSetup                              |
| M2   | Acc -> Controller  | SRP salt (16 bytes), SRP public key B                  |
| M3   | Controller -> Acc  | SRP public key A, SRP proof M1                         |
| M4   | Acc -> Controller  | SRP proof M2                                           |
| M5   | Controller -> Acc  | Ed25519 public key + identifier, encrypted with ChaCha20-Poly1305 |
| M6   | Acc -> Controller  | Accessory Ed25519 public key + identifier, encrypted   |

**SRP-6a parameters (HAP-modified)**:
- Username: `"Pair-Setup"` (fixed)
- Password: 8-digit setup PIN (e.g. `"123-45-678"`)
- Group: 3072-bit per RFC 5054
- Hash: SHA-512
- Key derivation: HKDF-SHA-512

### Session — Pair Verify (Every Connection)

4-step TLV8 exchange over HTTP POST to `/pair-verify`:

| Step | Direction          | Content                                                |
|------|--------------------|--------------------------------------------------------|
| M1   | Controller -> Acc  | Ephemeral Curve25519 public key                        |
| M2   | Acc -> Controller  | Ephemeral Curve25519 public key, Ed25519-signed payload, encrypted |
| M3   | Controller -> Acc  | Ed25519-signed payload, encrypted                      |
| M4   | Acc -> Controller  | Verification result                                    |

After Pair Verify, a shared secret is established. All subsequent HTTP traffic on the TCP connection is encrypted frame-by-frame:

```
[2-byte LE length] [encrypted payload] [16-byte Poly1305 auth tag]
```

Encryption: ChaCha20-Poly1305 with incrementing nonces (per-direction counters).

### HTTP Endpoints (Within Encrypted Session)

| Endpoint           | Method | Purpose                                       |
|--------------------|--------|-----------------------------------------------|
| `/accessories`     | GET    | Full accessory database as JSON               |
| `/characteristics` | GET    | Read specific characteristics by `?id=AID.IID` |
| `/characteristics` | PUT    | Write characteristic values / subscribe to events |
| `/pair-setup`      | POST   | Pair Setup exchange (TLV8)                    |
| `/pair-verify`     | POST   | Pair Verify exchange (TLV8)                   |
| `/pairings`        | POST   | Manage pairings (add/remove/list controllers) |

### Persistent Storage

Must survive restarts:
- Accessory Ed25519 long-term keypair
- Device ID (MAC-like identifier, generated once)
- Paired controller records (controller ID, Ed25519 public key, permissions)
- Configuration number

File-based storage in Chipi's data directory (e.g. `/var/chipi/homekit/pairing.store` or configurable path).

## Accessory Model

### HomeKit Data Hierarchy

```
Bridge (AID=1)
 └── Accessory (AID=2..N)
      └── Service (e.g. Lightbulb, Thermostat)
           └── Characteristic (e.g. On, Brightness, CurrentTemperature)
                ├── IID (instance ID, unique within accessory)
                ├── Type (UUID)
                ├── Value
                ├── Permissions (read, write, notify)
                └── Format (bool, int, float, string, tlv8)
```

### Supported Accessory Types (Initial Set)

Phase 1 — core types:

| Type              | Service UUID | Required Characteristics                          |
|-------------------|-------------|---------------------------------------------------|
| Lightbulb         | `43`        | On (bool)                                          |
| Switch            | `49`        | On (bool)                                          |
| Outlet            | `47`        | On (bool), OutletInUse (bool)                      |
| Temperature Sensor| `8A`        | CurrentTemperature (float)                         |
| Humidity Sensor   | `82`        | CurrentRelativeHumidity (float)                    |
| Contact Sensor    | `80`        | ContactSensorState (uint8)                         |
| Motion Sensor     | `85`        | MotionDetected (bool)                              |

Phase 2 — extended types:

| Type              | Service UUID | Required Characteristics                          |
|-------------------|-------------|---------------------------------------------------|
| Thermostat        | `4A`        | CurrentTemp, TargetTemp, CurrentMode, TargetMode, DisplayUnits |
| Window Covering   | `8C`        | CurrentPosition, TargetPosition, PositionState     |
| Lock Mechanism    | `45`        | LockCurrentState, LockTargetState                  |
| Fan               | `B7`        | Active                                             |
| Garage Door       | `41`        | CurrentDoorState, TargetDoorState, ObstructionDetected |
| Leak Sensor       | `83`        | LeakDetected                                       |
| Smoke Sensor      | `87`        | SmokeDetected                                      |
| Battery Service   | `96`        | BatteryLevel, ChargingState, StatusLowBattery      |
| Stateless Switch  | `89`        | ProgrammableSwitchEvent, ServiceLabelIndex         |

### Optional Characteristics (Common)

| Characteristic    | Applies To    | Format          |
|-------------------|---------------|-----------------|
| Brightness        | Lightbulb     | int 0-100       |
| Hue               | Lightbulb     | float 0-360     |
| Saturation        | Lightbulb     | float 0-100     |
| ColorTemperature  | Lightbulb     | int 140-500     |
| Name              | All services  | string          |
| StatusActive      | Sensors       | bool            |
| StatusLowBattery  | Sensors       | uint8           |

## Item-to-Accessory Mapping

### Design Pattern (Inspired by openHAB)

openHAB maps items to HomeKit via metadata tags. We adopt a similar approach using binding parameters.

### Simple Accessories — Single Item

One Chipi item maps to one HomeKit accessory with one service:

```lisp
(defitem 'kitchen-light "Kitchen Light" 'boolean
  (knx-binding :ga "1/2/3" :dpt "1.001")
  (homekit-binding :service :lightbulb))
```

The binding infers that the item's boolean value maps to the `On` characteristic. Type mapping:

| Chipi Type | HomeKit Format | Conversion                          |
|------------|----------------|-------------------------------------|
| `boolean`  | `bool`         | `item:true` -> `true`, `item:false` -> `false` |
| `number`   | `float`/`int`  | Direct numeric value                |
| `string`   | `string`       | Direct string value                 |

### Complex Accessories — Itemgroup

A Chipi itemgroup maps to one HomeKit accessory with one or more services. Each child item specifies its characteristic:

```lisp
(defitemgroup 'living-thermostat "Living Room Thermostat"
  (defitem 'lr-current-temp "Current Temp" 'number
    (knx-binding :ga "3/1/0" :dpt "9.001")
    (homekit-binding :service :thermostat
                     :characteristic :current-temperature))
  (defitem 'lr-target-temp "Target Temp" 'number
    (knx-binding :ga "3/1/1" :dpt "9.001")
    (homekit-binding :service :thermostat
                     :characteristic :target-temperature))
  (defitem 'lr-mode "Mode" 'number
    (knx-binding :ga "3/1/2" :dpt "5.010")
    (homekit-binding :service :thermostat
                     :characteristic :target-heating-cooling-state)))
```

Items sharing the same `:service` within a group are combined into a single HomeKit service on the same accessory.

### Accessory ID Generation

Stable IDs are critical — changing an accessory's AID breaks the HomeKit pairing for that accessory. IDs are generated by hashing the Chipi item name (symbol), ensuring stability across restarts.

### Configuration Parameters

```lisp
(homekit-binding :service :lightbulb
                 :characteristic :brightness
                 :min-value 0
                 :max-value 100
                 :step 1
                 :inverted nil)
```

| Parameter       | Purpose                                           |
|-----------------|---------------------------------------------------|
| `:service`      | HomeKit service type keyword                      |
| `:characteristic` | HomeKit characteristic type (optional for simple) |
| `:min-value`    | Override minimum value for numeric characteristics |
| `:max-value`    | Override maximum value                            |
| `:step`         | Value granularity                                 |
| `:inverted`     | Reverse boolean/percentage logic                  |

## DSL Integration

### Initialization

```lisp
(defconfig "my-home"
  (homekit-init :name "Chipi Bridge"
                :pin "123-45-678"
                :port 51826
                :storage-path #P"/var/chipi/homekit/"))
```

`homekit-init`:
- Generates or loads Ed25519 keypair and device ID from storage
- Starts the HAP HTTP server on the specified port
- Starts mDNS advertising
- Registers a shutdown hook via `hab:add-to-shutdown`

### Shutdown

`homekit-shutdown`:
- Stops mDNS advertising (kills subprocess)
- Closes all encrypted sessions
- Stops HAP HTTP server
- Persists state

Called automatically during `hab:shutdown`.

### Public API

```lisp
;; Macro: create a HomeKit binding for use in defitem
(homekit-binding &rest args &key service characteristic &allow-other-keys)

;; Initialize HomeKit bridge
(homekit-init &key name pin port storage-path)

;; Shutdown HomeKit bridge
(homekit-shutdown)
```

## Implementation Components

### File Structure

```
bindings/homekit/
├── binding-homekit.asd          # ASDF system definition
├── tlv8.lisp                    # TLV8 encoder/decoder
├── hap-crypto.lisp              # SRP-6a, HKDF, ChaCha20-Poly1305 AEAD
├── hap-session.lisp             # Encrypted session framing (usocket)
├── hap-pairing.lisp             # Pair Setup + Pair Verify state machines
├── hap-server.lisp              # Minimal HTTP server for HAP endpoints
├── hap-mdns.lisp                # mDNS advertising via avahi/dns-sd subprocess
├── hap-persistence.lisp         # Pairing store (file-based)
├── accessory-model.lisp         # Service/characteristic definitions, CLOS classes
├── binding-homekit.lisp         # Chipi binding integration, item<->accessory mapping
└── test/
    ├── binding-homekit-test-suite.lisp
    ├── tlv8-test.lisp
    ├── hap-crypto-test.lisp
    ├── hap-pairing-test.lisp
    ├── hap-server-test.lisp
    ├── accessory-model-test.lisp
    └── binding-homekit-test.lisp
```

### ASDF System

```lisp
(defsystem "binding-homekit"
  :version "0.1.0"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("chipi"
               "ironclad"       ; All cryptographic primitives
               "usocket"        ; TCP server for HAP
               "flexi-streams") ; Octet stream handling
  :components ((:file "tlv8")
               (:file "hap-crypto")
               (:file "hap-session" :depends-on ("hap-crypto" "tlv8"))
               (:file "hap-pairing" :depends-on ("hap-crypto" "hap-session" "tlv8"))
               (:file "hap-mdns")
               (:file "hap-persistence")
               (:file "accessory-model")
               (:file "hap-server" :depends-on ("hap-session" "hap-pairing"
                                                 "hap-mdns" "hap-persistence"
                                                 "accessory-model"))
               (:file "binding-homekit" :depends-on ("hap-server" "accessory-model"))))
```

### Component Details

#### `tlv8.lisp` — TLV8 Encoder/Decoder

Simple Type-Length-Value encoding with 8-bit type and length fields. Values longer than 255 bytes are split across multiple TLV entries with the same type (continuation).

```lisp
(defun tlv8-encode (alist) ...)   ; '((type . #(bytes...))) -> octet vector
(defun tlv8-decode (octets) ...)  ; octet vector -> '((type . #(bytes...)))
```

#### `hap-crypto.lisp` — Cryptographic Operations

All built on Ironclad:

- **SRP-6a** (HAP variant): `srp-generate-salt`, `srp-compute-verifier`, `srp-server-start`, `srp-server-verify`, `srp-server-proof`. Uses 3072-bit group, SHA-512, password = PIN.
- **HKDF-SHA-512**: `hkdf-derive-key` wrapping Ironclad's HMAC for extract and expand steps (per RFC 5869).
- **ChaCha20-Poly1305 AEAD**: `aead-encrypt`, `aead-decrypt` composing Ironclad's ChaCha20 cipher and Poly1305 MAC.
- **Ed25519**: Thin wrappers around Ironclad for key generation, signing, verification.
- **Curve25519**: Thin wrappers for ephemeral DH key exchange.

#### `hap-session.lisp` — Encrypted Session Framing

Wraps a usocket TCP connection with encrypt/decrypt framing:

- Read: `[2-byte LE length][encrypted payload][16-byte auth tag]` -> decrypt -> plaintext HTTP
- Write: plaintext HTTP -> encrypt -> `[2-byte LE length][encrypted payload][16-byte auth tag]`
- Per-direction nonce counters (64-bit, little-endian, incrementing)

#### `hap-pairing.lisp` — Pairing State Machines

Two state machines:

- `pair-setup-handler`: Processes M1/M3/M5, responds with M2/M4/M6. On success, stores controller's Ed25519 public key.
- `pair-verify-handler`: Processes M1/M3, responds with M2/M4. On success, returns session encryption keys.
- `pairings-handler`: Add/remove/list paired controllers.

#### `hap-server.lisp` — HAP HTTP Server

Minimal HTTP server built on usocket (not Hunchentoot — HAP's encrypted framing is incompatible with Hunchentoot's connection model):

- Listens on configured port
- Accepts TCP connections
- Before pairing: serves `/pair-setup`, `/pair-verify` in plaintext
- After Pair Verify: all traffic goes through encrypted session framing
- Routes: `/accessories`, `/characteristics`, `/pair-setup`, `/pair-verify`, `/pairings`
- Runs in a dedicated Sento actor for thread safety

#### `hap-mdns.lisp` — mDNS Advertising

Manages a subprocess for service advertising:

- Linux: `avahi-publish-service "name" _hap._tcp <port> <txt-records...>`
- macOS: `dns-sd -R "name" _hap._tcp local <port> <txt-records...>`
- Detects platform at runtime
- Restarts subprocess when configuration number changes (re-advertise)
- Kills subprocess on shutdown

#### `hap-persistence.lisp` — Pairing Store

File-based persistent storage:

```lisp
(defclass pairing-store ()
  ((path :initarg :path)
   (device-id :accessor device-id)         ; "XX:XX:XX:XX:XX:XX"
   (ed25519-keypair :accessor ed25519-keypair)
   (paired-controllers :accessor paired-controllers) ; hash-table: id -> (pubkey . permissions)
   (config-number :accessor config-number)))

(defgeneric load-store (store))
(defgeneric save-store (store))
```

Uses `jzon` for JSON serialization with base64-encoded key material.

#### `accessory-model.lisp` — Service/Characteristic Definitions

CLOS classes modeling the HomeKit accessory database:

```lisp
(defclass hap-accessory ()
  ((aid :reader aid)
   (services :accessor services)))

(defclass hap-service ()
  ((type-id :reader type-id)       ; UUID string
   (characteristics :accessor characteristics)))

(defclass hap-characteristic ()
  ((iid :reader iid)
   (type-id :reader type-id)
   (format :reader char-format)    ; :bool, :int, :float, :string, :uint8
   (permissions :reader permissions) ; list of :read, :write, :notify
   (value :accessor char-value)
   (min-value :reader min-value)
   (max-value :reader max-value)
   (step :reader char-step)))
```

Registry of known service types and their mandatory/optional characteristics, defined as data:

```lisp
(defparameter *service-definitions*
  '((:lightbulb
     :uuid "43"
     :required ((:on :uuid "25" :format :bool :perms (:read :write :notify)))
     :optional ((:brightness :uuid "8" :format :int :min 0 :max 100 :perms (:read :write :notify))
                (:hue :uuid "13" :format :float :min 0 :max 360 :perms (:read :write :notify))
                (:saturation :uuid "2F" :format :float :min 0 :max 100 :perms (:read :write :notify))))
    (:switch
     :uuid "49"
     :required ((:on :uuid "25" :format :bool :perms (:read :write :notify))))
    ;; ... etc
    ))
```

#### `binding-homekit.lisp` — Chipi Integration

The binding class and DSL macro:

```lisp
(defclass homekit-binding (binding:binding)
  ((service-type :reader service-type :initarg :service-type)
   (characteristic-type :reader characteristic-type :initarg :characteristic-type)
   (config :reader binding-config :initarg :config)))
```

Global registry (similar pattern to KNX):
- `*homekit-bridge*` — the running HAP server instance
- `*item-accessory-map*` — hash-table mapping Chipi item names to (AID . IID) pairs

State synchronization:
- **Push function**: Called when Chipi item changes → updates HAP characteristic value → sends event notification to subscribed controllers
- **Read handler**: Called when HomeKit controller reads characteristic → reads current Chipi item state
- **Write handler**: Called when HomeKit controller writes characteristic → calls `item:set-value` on Chipi item

## Build Order

Recommended implementation sequence:

| Phase | Components                              | Testable Outcome                              |
|-------|-----------------------------------------|-----------------------------------------------|
| 1     | `tlv8.lisp`, `hap-crypto.lisp`         | Unit tests for encoding, SRP-6a, AEAD        |
| 2     | `hap-persistence.lisp`                  | Store/load pairing data                       |
| 3     | `hap-session.lisp`, `hap-pairing.lisp` | Pair Setup/Verify against test vectors        |
| 4     | `hap-server.lisp`, `hap-mdns.lisp`     | Discoverable + pairable from iOS              |
| 5     | `accessory-model.lisp`                  | Serve `/accessories` with test data           |
| 6     | `binding-homekit.lisp`                  | Full integration: Chipi items visible in Home app |
| 7     | Extended accessory types                | Thermostat, window covering, sensors, etc.    |

## Dependencies

| Library         | Purpose                              | Pure CL | Already in Chipi |
|-----------------|--------------------------------------|---------|-----------------|
| `ironclad`      | Ed25519, Curve25519, ChaCha20-Poly1305, SHA-512, HKDF, SRP bignum math | Yes | No |
| `usocket`       | TCP server for HAP                   | Yes     | Yes (via Hunchentoot) |
| `flexi-streams` | Octet stream handling                | Yes     | Yes (via Hunchentoot) |
| `com.inuoe.jzon`| JSON encoding for accessory database | Yes     | Yes |
| `alexandria`    | Utility functions                    | Yes     | Yes |
| `sento`         | Actor for HAP server thread safety   | Yes     | Yes |

Only `ironclad` is a new dependency. All others are already available.

## Reference Implementations

For porting protocol details:

| Implementation | Language | Best For                              |
|----------------|----------|---------------------------------------|
| [HAP-python](https://github.com/ikalchev/HAP-python) | Python | Clean SRP-6a and pairing code to port |
| [hap (Go)](https://github.com/brutella/hap) | Go | Well-structured crypto packages       |
| [openHAB HomeKit addon](https://github.com/openhab/openhab-addons) | Java | Item-to-accessory mapping pattern (via HAP-Java) |
| [HAP-NodeJS](https://github.com/homebridge/HAP-NodeJS) | TypeScript | Most complete reference, powers Homebridge |
| [Apple HomeKitADK](https://github.com/apple/HomeKitADK) | C | Official spec reference (archived, Apache-2.0) |
