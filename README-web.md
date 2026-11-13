# Chipi-API

Chipi-API adds an optional REST/JSON layer on top of the **Chipi** home-automation
framework. It lets you read and update item values via HTTP; every request is
authorised with scope-based API keys.

> Licence: Apache 2.0 (see root‐level `LICENSE`).

---

## Prerequisites

* SBCL ≥ 2.x (or any other supported Common Lisp implementation)  
* Quicklisp (or OCICL)  
* ASDF systems `chipi` **and** `chipi-api`

```lisp
;; Quicklisp
(ql:quickload :chipi-api)
;; OCICL
(asdf:load-system :chipi-api)
```

---

## Quick start (REPL)

```lisp
(hab:defconfig "chipi"
  ;; 1 – initialises runtime, actor system, timers …
  ;; 2 – Chipi-API specific environment
  (api-env:init
    :apikey-store (apikey-store:make-simple-file-backend) ; or *memory-backend* for testing
    :apikey-lifetime (ltd:duration :day 100))

  ;; 3 – HTTP server on port 8765
  (api:start))
```

For a complete, runnable example have a look at
[example-web.lisp](./example-web.lisp) in the project root.

### Example items

```lisp
(defitem 'lamp  "Living-room lamp" 'boolean :initial-value 'item:false)
(defitem 'temp  "Temperature"      'float   :initial-value 21.5)
```

### Read-only items

Items can be marked as read-only for external systems by setting the `:ext-readonly` tag:

```lisp
(defitem 'sensor "Temperature sensor" 'float 
  :initial-value 20.0
  :tags '((:ext-readonly . t)))
```

Read-only items can still be read via the REST API but cannot be updated through external calls. The tag value should be the Lisp boolean `t`.

**Note:** This is a convention that should be respected by the UI and external systems. The server does not enforce read-only restrictions - it's up to client implementations to check for the `:ext-readonly` tag and prevent updates accordingly.

---

## Creating an API key

```lisp
(defparameter *my-key*
  (apikey-store:create-apikey :access-rights '(:read :update)))
```

### API key persistence

```lisp
(api-env:init
  :apikey-store (apikey-store:make-simple-file-backend))
```

The file backend stores keys in `runtime/apikeys`.

---

## REST interface

A full OpenAPI 3.0 specification lives in `chipi-api.yaml`.

Besides individual items the API also exposes *itemgroups*, logical
containers that hold multiple items.

| Endpoint            | Method | Required scope |
|---------------------|--------|----------------|
| `/items`            | GET    | `read`         |
| `/items/{itemName}` | GET    | `read`         |
| `/items/{itemName}` | POST   | `update`       |
| `/itemgroups`            | GET    | `read`         |
| `/itemgroups/{groupName}` | GET    | `read`         |
| `/events/items`     | GET    | `read`         |

Add header `X-Api-Key: <your-key>` to every call.

### Examples (curl)

```bash
# List all items
curl -H "X-Api-Key: $MY_KEY" http://localhost:8765/items

# List all itemgroups
curl -H "X-Api-Key: $MY_KEY" http://localhost:8765/itemgroups

# Get single item
curl -H "X-Api-Key: $MY_KEY" http://localhost:8765/items/lamp

# Get single itemgroup
curl -H "X-Api-Key: $MY_KEY" http://localhost:8765/itemgroups/living

# Update item
curl -X POST -H "X-Api-Key: $MY_KEY" -H "Content-Type: application/json" \
     -d '{"value": true}' http://localhost:8765/items/lamp
```

### Server-Sent Events (SSE)

The API provides real-time item updates via Server-Sent Events:

```bash
# Connect to item events stream
curl -H "Accept: text/event-stream" \
     "http://localhost:8765/events/items?apikey=$MY_KEY"
```

The SSE endpoint sends:
- Connection confirmation messages
- Real-time item change notifications with full item data
- Periodic heartbeat messages to keep the connection alive

Event format follows the SSE standard with `data:` fields containing JSON payloads.

#### Event Types

**Connection Event:**
```
{"event":{"type":"connection","message":"Connected to item events"}}
```

**Item Change Event:**
```
{"event":{"type":"item-change","item":{"name":"lamp","label":"Living-room lamp","type-hint":"boolean","tags":{},"item-state":{"value":true,"timestamp":1703123456}}}}
```

**Heartbeat Event:**
```
{"event":{"type":"heartbeat","timestamp":1703123456}}
```

#### Item Change Payload Structure

| Field | Type | Description |
|-------|------|-------------|
| `event.type` | string | Always `"item-change"` |
| `event.item.name` | string | Item identifier |
| `event.item.label` | string | Human-readable item name |
| `event.item.type-hint` | string | Data type (`boolean`, `float`, `integer`, `string`) |
| `event.item.tags` | object | Item metadata (including `:ext-readonly` flag) |
| `event.item.item-state.value` | any | Current item value |
| `event.item.item-state.timestamp` | number | Common Lisp universal-time timestamp (seconds since 1900-01-01) |

#### Timestamp Format

Timestamps are provided as Common Lisp universal-time values (seconds since January 1st, 1900, 00:00:00 GMT). To convert to Unix timestamp (seconds since 1970-01-01), subtract 2208988800:

```javascript
// Convert CL universal-time to Unix timestamp
const unixTimestamp = universalTime - 2208988800;
const date = new Date(unixTimestamp * 1000);
```

```bash
# Convert in shell (example with timestamp 3913123456)
echo $((3913123456 - 2208988800))  # Result: 1704134656 (Unix timestamp)
```

---

## Scope model

| Scope  | Meaning            |
|--------|--------------------|
| `read` | read-only          |
| `update` | push new values  |
| `delete` | reserved         |
| `admin` | full access       |

The highest requested scope must not exceed the highest scope granted to the
API key.

---

## Shutting down

```lisp
(api:stop)     ; stop only the web API
(hab:shutdown) ; stop the entire Chipi instance
```


# Chipi UI

Chipi UI is a responsive web UI, based on CLOG and Bootstrap 5, with realtime
updates of values. It offers two modes that can be combined:

* an **auto-generated overview** at `/` that renders all itemgroups and items,
  driven by UI tags (see below) — zero configuration;
* **pages** declared with `defpage`: explicitly composed views built from
  widgets (toggles, inputs, sliders, charts, …), each served at its own URL —
  a wall-panel tablet bookmarks `/wall`, a phone `/mobile`, and so on.

Example of setup:

```lisp
(hab:defconfig "chipi"
  ;; neither api-env:init nor api:start are required for the UI
  (ui:start :host "localhost" :port 8080))
```

For a complete, runnable example (items, pages, a chart with demo data) see
[example-web.lisp](./example-web.lisp).

## Pages

Pages are defined with the `defpage` macro from package `chipi-ui.page`
(nickname `page`), typically via `(:use :cl :hab :page)` in the config
package:

```lisp
(defpage 'wall-panel "Wall panel"
  :path "/wall"                    ; optional; defaults to "/<id>" downcased
  :title "Ground floor"            ; optional page heading + browser tab title
  (section "Lights"
    (toggle 'switch1)
    (value 'motion-sensor :label "Motion"))
  (section "Climate"
    (value 'outside-temp :format "~,1f °C")
    (setpoint 'target-temp :min 15.0 :max 25.0 :step 0.5)
    (chart 'outside-temp :range '(:hours 6)))
  (page-link 'cellar))

(defpage 'cellar "Cellar"
  ;; embeds an existing itemgroup, rendered like on the overview
  (itemgroup-ref 'plugs))
```

Semantics:

* The page `label` is the short name used by `page-link`s pointing at the
  page; `:title`, when given, renders as the page heading and becomes the
  browser tab title. Without `:title` no heading is rendered (useful for
  space-constrained panels).
* Every page is directly reachable (deep-linkable) at its `:path`. A page
  with `:path "/"` replaces the auto-generated overview; without one the
  overview stays on `/`. Either is only the *default* for `/` — each device
  can pick its own home page, see [Settings](#settings).
* `page-link` navigation pushes browser history: back/forward work, a back
  button appears in the app header on navigated pages, and a reload stays on
  the current page.
* Going back returns to the view as it was left — same scroll position,
  charts not reloaded, values current: the views of the history entries next
  to the current one (two in either direction) stay alive, hidden, and keep
  receiving value updates. Entries further away, a page re-defined in the
  meantime, and a reload render afresh, still at the scroll position they
  were left at.
* Every view carries an app header: back (once navigated), the app name, a
  home button and the settings gear. Installed as a web app there is no
  browser chrome, so this is the navigation the user gets. On a locked
  device (see [Settings](#settings)) the gear is left out.
* Re-defining a page (same id) replaces it, like the other `def*` macros.
* Widgets are ordinary values, so pages can be composed programmatically:

```lisp
(apply #'section "All temperatures"
       (mapcar (lambda (id) (value id :format "~,1f °C"))
               '(cellar-temp boiler-temp freezer-temp)))
```

### Widgets

| Widget | Item type | Description |
|--------|-----------|-------------|
| `(value item &key label format)` | any | Read-only display; `format` is a CL format string (e.g. `"~,1f °C"`); booleans render as ON/OFF |
| `(toggle item &key label)` | boolean | Switch control |
| `(text-input item &key label)` | string | Text field |
| `(number-input item &key label min max step)` | integer/float | Number entry field |
| `(slider item &key label min max step)` | integer/float | Slider with a live value label (defaults 0–100, step 1) |
| `(setpoint item &key label min max step)` | integer/float | Stepper with −/+ buttons, e.g. for target temperatures |
| `(selection item &key label choices)` | string/integer/float | Dropdown; `choices` is an alist of `(value . label)`, the value written in the item's type |
| `(chart item-or-items &key label type range persistence transform fill line-width height refresh right-axis)` | number/boolean | History chart, one series per item (see below) |
| `(button caption action &key label)` | — | Push-button running `action` (see below) |
| `(button-group label &rest buttons)` | — | Several buttons sharing one row and label |
| `(section label &rest widgets)` | — | Titled card grouping widgets |
| `(row &rest widgets)` | — | Lays its widgets side by side in equal columns (see below) |
| `(page-link page-id &key label)` | — | Navigation link to another page |
| `(itemgroup-ref group-id)` | — | Embeds an itemgroup as an overview-style card |

The widget `label` always defaults to the item's own label. Unknown item or
page references render an inline "Unknown …" placeholder instead of breaking
the page.

### Horizontal rows

Widgets normally stack, one per line. `row` puts several next to each other in
equally wide columns — one column per child, so two children render 50/50 and
three 33/33/33:

```lisp
(section "Status"
  (row
    (chart 'outside-temp :label "Außentemperatur [°C]" :range '(:hours 12))
    (chart 'cistern-level :label "Füllgrad Zisterne [%]" :range '(:days 7))))
```

* `:columns` sets the count explicitly and wraps the children over several
  lines when they outnumber it, e.g. `(row :columns 2 …)` with four charts
  renders a 2×2 block.
* Below 768px the row collapses to a single column, so a phone still gets one
  widget per line.
* Any widget works, not just charts.

A row does **not** have to be inside a section — it is an ordinary widget and
goes anywhere one can, including directly under `defpage`. A page-level row of
sections puts two cards next to each other, which is otherwise impossible:
every section is full width on its own.

```lisp
(defpage 'technik "Technik"
  :title "Technik"
  (row (section "Kessel" (value 'boiler-temp))
       (section "Puffer" (value 'buffer-temp)))
  (page-link 'cellar))
```

One caveat when going section-less: the white card comes from `section`, not
from `row`. Bare item widgets in a page-level row sit directly on the page
background with no card around them — put a `section` in the row (as above),
or the row in a `section`, unless that is what you want.

Charts are the main beneficiary: a single-series chart over a short range is
mostly empty space at full width. A chart with many series is the opposite
case and stays easier to read wide — halving its width also halves the room
its time axis has.

### Action buttons

`button` is the one widget that is not bound to an item: clicking it calls a
function. That is what **momentary commands** need — a door opener, a
shutter's Auf/Stopp/Ab — because `item:set-value` is a no-op when the value
does not change, so an item-bound control cannot send the same command twice
in a row. The function is free to go through an item all the same:
`house:send-command` pushes to the item's bindings every time, so the
action stays independent of the binding behind the item.

```lisp
(section "Jalousie Wohnzimmer"
  (button-group "Auf/Stopp/Ab"
    (button "▲" 'jal-wz-up)
    (button "■" 'jal-wz-stop)
    (button "▼" 'jal-wz-down))
  (slider 'jal-wz-pos :label "Position [%]"))

;; a single button; :label renders to the left, like an item widget's label
(button "Öffnen" 'tueroeffner-trigger :label "Türöffner")
```

* The action takes no arguments and its return value is ignored. An error it
  signals is logged (`Button action failed …`) and does not reach the browser
  connection, so a failing command cannot break the page.
* A **symbol** naming a function is resolved on every click, so redefining
  that function — from the REPL, over Slynk, against a running instance —
  takes effect without re-evaluating the page. `#'the-function` captures the
  function as of page definition instead.
* CLOG dispatches every browser event on its own thread, so an action that
  blocks (a bus write waiting for its ack) does not stall the UI.
* The action runs whenever anyone reaching the UI clicks the button; the UI
  has no per-widget authorization, so treat a button exactly like the
  toggles next to it.

### Charts

`chart` plots one or more items' history from a **historic persistence**
(e.g. influx) and appends new values in realtime as the items change.

* Passing a list of items renders one series per item in a shared,
  timestamp-aligned plot with a legend.  List elements are item ids or
  `(item-id . "Series label")` conses; series labels default to the item
  labels:

```lisp
(chart '((kessel-temp . "Kessel")
         (puffer-temp . "Puffer")
         boiler-temp)
       :label "Heizung" :range '(:hours 12))
```

* `:range` — either a plist passed to `persp:make-relative-range`, e.g.
  `'(:hours 6)` or `'(:days 1)` (default: last day), or a ready
  `persp:range` object.
* `:type` — `:line` (default) or `:bar`.
* `:persistence` — the persistence id to load from; without it the first
  defined historic persistence is used.
* `:transform` — a one-argument function applied to every charted value,
  historic and live alike, e.g. to chart a raw sensor reading in a display
  unit: `(chart 'level-sensor :transform (lambda (ma) (- (* 18.87 ma) 147.19)))`.
  It is called with a number (booleans chart as 1/0) and must return a
  number; a failing transform charts the point as a gap.
* `:fill` — the translucent area between each line and zero, in the series'
  own colour. `:auto` (default) fills a lone series and leaves a multi-series
  chart as bare lines; `t` fills every series (the fills overlap, fine for a
  few power flows around zero, mud for nine temperatures); `nil` never fills.
* `:line-width` — stroke width of every series in CSS pixels (default 2).
* `:right-axis` — plots some of the series against a second y-axis at the
  right edge, with a scale of its own, for a series whose unit differs from
  the rest — a battery's state of charge in % among power flows in W would
  otherwise be a flat line at the bottom of a scale in thousands:

  ```lisp
  (chart '((pv-power . "PV")
           (grid-power . "Netz")
           (battery-soc . "Batterie [%]"))
         :label "Energie [W]"
         :right-axis '(:series (battery-soc) :range (0 100)))
  ```

  `:series` lists the item ids that read against the right axis; `:range`
  fixes its bounds as `(min max)` (without it the axis auto-scales, which for
  a percentage lets 60..80 fill the plot height). Right-axis series are drawn
  dashed and never filled, so they can be told from the left-axis ones without
  the legend; a lone right-axis series lends the axis its colour. A series id
  not in the chart, or a range with `min >= max`, is an error at page
  definition.
* `:height` — plot height in CSS pixels (default 220); the width follows the
  container.
* `:refresh` — minimum seconds between two live-appended points of one
  series, for items that broadcast every few seconds. The history load is
  unaffected.
* Points (markers) on a line are uPlot's own doing: it draws them only when
  the data is sparse enough that consecutive samples sit further apart than a
  marker, i.e. a lone series persisted every 30 minutes over 12 hours shows
  its samples, a dense one does not.
* Items without a historic persistence render a "No history available"
  placeholder. `example-web.lisp` contains a small in-memory historic
  persistence that seeds demo data for the chart.
* Charts are rendered with [uPlot](https://github.com/leeoniya/uPlot). uPlot,
  Bootstrap and jQuery are served by chipi itself from
  `ui/static-files/vendor/`, so neither the server nor the browser needs
  internet access.

## Settings

The gear in the app header opens `/settings`. Settings are **per device**:
they live in the browser's `localStorage`, so a wall tablet, a phone and a
desktop browser can each keep their own — nothing is stored on the server.

| Setting | Description |
|---------|-------------|
| Home page | The page `/` shows on this device: any `defpage`, or *Default* — the page with `:path "/"` if there is one, otherwise the auto-generated overview. A stored page that no longer exists falls back to the default. |
| Device lock | Hides the settings gear on this device and puts a PIN prompt in front of `/settings`. Only offered when `ui:start` was given a `:settings-pin`. |

`/settings` is served by the UI itself; a `defpage` claiming that path is
not reachable (a warning is logged).

### Locking a device

For a device that should show one page and stay on it — a wall panel, or the
kids' tablet — configure a PIN and lock the device:

```lisp
(ui:start :host "0.0.0.0" :port 8080 :settings-pin "4711")
```

Then, on the device: gear → pick its home page → *Lock this device*. From
then on the gear is gone. To get back into the settings, tap the *Chipi*
name in the header five times within three seconds (the only way in an
installed app, which has no address bar), or open `/settings` in a browser;
either way the PIN prompt comes first. The PIN is compared on the server and
never sent to the device. A correct PIN unlocks the settings for that
connection only — a reload asks again — and *Unlock this device* removes
the lock.

Two limits worth knowing:

* The lock is a `localStorage` flag like the home page. Clearing the site
  data or reinstalling the app resets the device to unlocked and the default
  home page, so make the page for `/` the one that is harmless to land on.
* The lock fixes the home page, not what the device can reach: every page
  its home page links to is still one tap away, and a browser can still type
  a page's URL.

Dropping `:settings-pin` from the config again unlocks every device: without
a PIN the stored flag is ignored, so no device is ever stranded without a way
into its settings.

### Sections from plugins

A plugin can add a view of its own under the settings -- the
[Matter binding](bindings/matter/README.md) adds *Share with other apps*,
with the bridge's pairing QR code, the apps that added the bridge and the
bridged devices (load `binding-matter/ui` next to `chipi-ui`). It registers
a section:

```lisp
(ui-settings:register-section :matter-bridge
                              :label "Share with other apps"
                              :render #'render-share-section   ; (ctx container) -> renders into container
                              :summary #'summary)              ; () -> the card's line, or nil
```

The settings view then shows a card per section, with the summary as its
line and a row that opens `/settings/<id>`; the view behind it carries the
same header and back button, and the device lock covers it like the settings
themselves. The path is registered as a route, so a reload or a typed URL
lands in it. The renderer gets the navigation context and an element of its
own to fill; whatever it renders may render itself afresh into that element,
as the Matter card does after a button. An error in the renderer or the
summary is logged and shown in the section's place, never signalled into the
UI. chipi-ui itself stays free of the plugin: the registration is the only
link. `ui-settings:unregister-section` takes a section away again.

## Access key

Without further ado the UI is open to everyone who can reach the port. An
access key puts a login page in front of all of it:

```lisp
(ui:start :host "0.0.0.0" :port 8080 :access-key "a-long-random-string")
```

A browser without a session then gets nothing — not the page, not the
websocket, not a stylesheet — but a redirect to `/login`, which asks for the
key. The right key answers with a session cookie and sends the browser on to
the page it asked for. From then on the cookie is all the browser needs; an
installed web app keeps it, so a wall tablet logs in once.

How it is built, and what that means for you:

* The cookie holds a signed token with an expiry, not the key. It is valid
  for `chipi-ui.auth:*session-life-time*` seconds (default 30 days); after
  that the next request lands on the login page again. The signing key is
  derived from the access key, so sessions survive a restart of the server,
  and changing the access key logs every device out.
* Failed logins are throttled per client address: three attempts are free,
  then each further failure doubles the wait before the next one, from 2
  seconds up to 15 minutes. A blocked attempt is refused at once with a
  *429 Too Many Requests* and the seconds to wait — the right key is not
  even looked at. A successful login clears the address's record, and so
  does a restart. Behind a reverse proxy every client shares the proxy's
  address and so its record; forwarding headers are deliberately not
  trusted. The knobs are `*free-attempts*`, `*base-delay*`, `*max-delay*`
  and `*forget-after*` in `chipi-ui.auth`.
* The manifest and the icons are served without a session: the browser
  fetches them without credentials when the app is installed, and neither
  is secret.
* Without TLS the key and the cookie cross the network in clear text, so
  on anything but a trusted LAN serve the UI over https, see [TLS](#tls).
  The cookie is then marked Secure and sent over https only.
* The key is compared in constant time, and logins are logged with the
  client address — a success at info, a failure or a refused attempt at
  warn.

The access key and the settings PIN are independent: the key decides who
gets in at all, the PIN decides who may change a locked device's settings.

## TLS

The UI is served over https when `ui:start` is given a certificate and its
key, both PEM files:

```lisp
(ui:start :host "0.0.0.0" :port 8443
          :access-key "a-long-random-string"
          :ssl-cert-file "/etc/chipi/chipi.crt"
          :ssl-key-file "/etc/chipi/chipi.key")
```

Both or neither: one without the other, or a file that is not there, is an
error at start rather than a server that silently comes up without TLS. The
websocket runs over the same connection (`wss://`), and the session cookie
is marked Secure.

TLS needs Hunchentoot built with `cl+ssl`, so the `:hunchentoot-no-ssl`
feature must not be set when chipi-ui is loaded. On AmiLisp that is the
default: its `cl+ssl` facade routes Hunchentoot's TLS through the native
TLS layer (OpenSSL on the host, AmiSSL on AmigaOS), and the UI serves
https there like anywhere else. Loading with the no-ssl feature, as some
AmiLisp test scripts do, leaves a Hunchentoot without the SSL acceptor,
and `ui:start` with the two files then fails at start.

Chipi does not make certificates. That is the installation's job, together
with getting the certificate's issuer trusted by the devices. For a home
setup the issuer is a private root CA that is installed on each device
once; what chipi needs from that is below.

### What the certificate has to satisfy

Browsers are strict about the server certificate even when they trust its
issuer. Apple's devices in particular refuse one that misses any of these,
and an installed web app shows no warning page to click through — it just
stays dead.

| Requirement | Why |
|-------------|-----|
| Issued by a CA the devices trust, not self-signed | A bare self-signed certificate cannot be trusted on iOS at all, and Safari refuses websockets to it. |
| A `subjectAltName` with every name the UI is opened by — hostname, `.local` name, IP address | The Common Name alone is ignored by all current browsers. |
| `extendedKeyUsage = serverAuth` | Required by Apple's and Chrome's policies. |
| Validity of at most 825 days | Longer leaf certificates are rejected by Apple devices. |
| RSA 2048 or longer, or an EC key, SHA-256 | SHA-1 and short keys are rejected. |

The root CA itself should be long-lived (ten years is fine) with
`basicConstraints = CA:TRUE`; the devices are told to trust it once and
need not see it again when the leaf is reissued.

As a reference for what the installation produces, this makes a root and a
leaf that meet the list with plain `openssl` (`mkcert` does the same in two
commands and also handles the local trust store):

```sh
# the root CA, once; keep rootCA.key out of reach, only rootCA.pem goes to the devices
openssl req -x509 -newkey rsa:3072 -nodes -sha256 -days 3650 \
  -subj "/CN=My Home CA" \
  -addext "basicConstraints=critical,CA:TRUE" \
  -addext "keyUsage=critical,keyCertSign,cRLSign" \
  -keyout rootCA.key -out rootCA.pem

# the server certificate, reissued before it expires
openssl req -new -newkey rsa:2048 -nodes -subj "/CN=chipi" \
  -keyout chipi.key -out chipi.csr
printf "subjectAltName=DNS:chipi.local,DNS:chipi,IP:192.168.1.10\n\
extendedKeyUsage=serverAuth\nbasicConstraints=CA:FALSE\n" > leaf.ext
openssl x509 -req -in chipi.csr -CA rootCA.pem -CAkey rootCA.key -CAcreateserial \
  -days 825 -sha256 -extfile leaf.ext -out chipi.crt
openssl verify -CAfile rootCA.pem chipi.crt
```

`chipi.crt` and `chipi.key` go to `ui:start`; the names in `subjectAltName`
are the ones to type into the browser.

### Trusting the root on the devices

Only `rootCA.pem` is distributed, never `rootCA.key`.

* **macOS**: double-click `rootCA.pem` to add it to the login keychain, open
  it in *Keychain Access*, and under *Trust* set *When using this
  certificate* to *Always Trust*. Safari and Chrome follow the keychain.
* **iOS / iPadOS**: get `rootCA.pem` onto the device (AirDrop, mail, or a
  download in Safari) and open it; iOS offers to install it as a profile,
  which is finished under *Settings → General → VPN & Device Management*.
  Then — this is the step that is easy to miss — enable it under
  *Settings → General → About → Certificate Trust Settings*. Without the
  second step the profile is installed but not trusted.
* **Android**: *Settings → Security → More security settings → Install from
  storage → CA certificate* (the exact path varies by vendor), pick the
  file, and accept the warning that a private CA is being added.

Once trusted, the UI opens at `https://chipi.local:8443/` without any
warning, and *Add to Home Screen* installs an app that works. A device that
shows a warning, or an installed app that stays empty, almost always means
the root is installed but not yet marked trusted, or the certificate lacks
the name the device used in its `subjectAltName`.

### Checking from the command line

```sh
# the issuer chain and the names in the certificate
openssl s_client -connect 192.168.1.10:8443 -servername chipi.local </dev/null 2>/dev/null \
  | openssl x509 -noout -issuer -ext subjectAltName -enddate
# the gate over https: expect a 302 to /login
curl --cacert rootCA.pem -o /dev/null -w "%{http_code} -> %{redirect_url}\n" https://chipi.local:8443/
```

A leaf certificate expires; put its end date in the calendar. Reissuing is
the second block above, followed by a restart of chipi. The root stays, so
the devices need no attention.

## Installing as an app

The UI is an installable web app: Safari's *Add to Home Screen* on an iPhone
or iPad, *Add to Dock* on a Mac, or Chrome's install prompt give it an icon
and open it full screen without browser chrome, launching on the device's
home page. The app is named *Chipi*; icons live in `ui/static-files/icons/`
and can be replaced with your own (180×180 for iOS, 192×192 and 512×512 for
the manifest).

Two things make this work that are worth knowing about:

* The manifest's `start_url` is always `/` — the launch URL is baked into the
  install, so the home page is a device setting (above) rather than part of
  the manifest, and changing it takes effect on the next launch without
  reinstalling.
* An installed app is suspended when the user switches away and its
  websocket dies with it. The server keeps a dropped session for
  `chipi-ui.main:*reconnect-delay*` seconds (default 120) so a short absence
  resumes in place; after a longer one the boot page notices the socket did
  not come back and reloads, which re-renders the current path.

Plain HTTP over the LAN is enough for installing; only service workers and
Web Push would need HTTPS, and the UI uses neither.

## UI Tags

Items and itemgroups support special tags that control how they are rendered in the UI.

### Item tags

| Tag | Value | Description |
|-----|-------|-------------|
| `:ui-type` | string | Overrides the default type badge label (e.g. "Light" instead of "Switch") |
| `:ui-readonly` | `t` | The item renders as a plain display instead of a control: a boolean as "ON"/"OFF" text instead of a toggle, an item of a [house kind](#house-kinds) as its value instead of the control of its kind |
| `:ui-order` | number | Position of the item in its itemgroup's card: items are listed ascending by it, items without the tag after every ordered one, ties in definition order |

```lisp
;; Custom type label
(defitem 'switch1 "Switch1" 'boolean :initial-value 'item:false
  :tags '((:ui-type . "Light")))

;; Read-only boolean displayed as ON/OFF text
(defitem 'motion-sensor "Motion Sensor" 'boolean :initial-value 'item:true
  :tags '((:ui-readonly . t)))
```

`:ui-order` is for a card that mixes kinds of items — a room with sockets,
lights, window contacts and its heating. Give every kind its own order in the
macro that defines it (sockets 10, lights 20, windows 30, …) and each room
card lists them kind by kind instead of in definition order, which is
whatever the config file happened to interleave.

### House kinds

An item defined with one of the `house` definers (`house:deflight`,
`house:defrollershutter`, …, see the chipi README) carries the tag
`:house-kind`. The type of an item says how its value is stored; the kind says
what it is, and an itemgroup card goes by it:

| Kind | A card row shows |
|------|------------------|
| `:light`, `:plug` | a toggle |
| `:contact` | "open" / "closed" in the state colours, never a toggle; a `:ui-mapping` replaces the two words |
| `:pulse` | a "Press" button, which presses and releases the item (`house:press`) |
| `:shutter-move` | ▲ and ▼ buttons |
| `:shutter-stop` | a ■ button |
| `:shutter-position`, `:shutter-slat` | a slider, 0–100 |
| `:setpoint` | a −/+ stepper, in steps of 0.5 (1 for an integer item) |
| `:mode` | a dropdown of the codes its `:ui-mapping` names; without a mapping, the value |
| `:temperature`, `:valve` | the value |

The buttons send *commands* (`house:send-command`): the same direction, or
another stop, goes out as often as it is pressed, which a toggle on the same
item could not do.

The type badge names the kind ("Light", "Shutter position", …) unless the item
has a `:ui-type`. `:ui-readonly` turns any of the controls back into a display
— for a position that is reported but must not be set from a card. The `value`
widget of a page reads a contact as "open" / "closed" as well.

```lisp
(defitemgroup 'living "Living room")

(house:defrollershutter blind-living "Blind"
  :move     ((knx-binding ...) :group '(living))    ; ▲ ▼
  :stop     ((knx-binding ...) :group '(living))    ; ■
  :position ((knx-binding ...) :group '(living)))   ; slider
```

### Itemgroup tags

| Tag | Value | Description |
|-----|-------|-------------|
| `:ui-link` | — | Renders the itemgroup as a clickable navigation link instead of an inline card |

```lisp
;; Renders as a navigation link that opens a detail page
(defitemgroup 'lights "Lights" :tags '((:ui-link)))
```

These tags drive the auto-generated overview and `itemgroup-ref` widgets;
explicit pages choose their widgets directly.

## Screenshots

The auto-generated itemgroup overview:

![Overview](docs/ui/UI.png)

A `defpage`-designed page — the UI is freely designable with widgets and
supports charts of item history:

![Page with widgets and chart](docs/ui/UI2.png)

The UI is under active development.
