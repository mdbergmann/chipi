# Chipi-Web

Chipi-Web adds an optional REST/JSON layer on top of the **Chipi** home-automation
framework. It lets you read and update item values via HTTP; every request is
authorised with scope-based API keys.

> Licence: Apache 2.0 (see root‐level `LICENSE`).

---

## Prerequisites

* SBCL ≥ 2.x (or any other supported Common Lisp implementation)  
* Quicklisp (or OCICL)  
* ASDF systems `chipi` **and** `chipi-web`

```lisp
;; Quicklisp
(ql:quickload :chipi-web)
;; OCICL
(asdf:load-system :chipi-web)
```

---

## Quick start (REPL)

```lisp
(hab:defconfig "chipi"
  ;; 1 – initialises runtime, actor system, timers …
  ;; 2 – Chipi-Web specific environment
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
  :tags '((:ext-readonly . "true")))
```

Read-only items will not show Toggle/Change buttons in the web UI, preventing accidental modifications of sensor values or other items that should only be updated by the system itself.

---

## Creating an API key

```lisp
(defparameter *my-key*
  (apikey-store:create-apikey :access-rights '(:read :update)))
```

---

## REST interface

A full OpenAPI 3.0 specification lives in `chipi-web-api.yaml`.

Besides individual items the API also exposes *itemgroups*, logical
containers that hold multiple items.

| Endpoint            | Method | Required scope |
|---------------------|--------|----------------|
| `/items`            | GET    | `read`         |
| `/items/{itemName}` | GET    | `read`         |
| `/items/{itemName}` | POST   | `update`       |
| `/itemgroups`            | GET    | `read`         |
| `/itemgroups/{groupName}` | GET    | `read`         |

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

## API-key persistence

```lisp
(api-env:init
  :apikey-store (apikey-store:make-simple-file-backend))
```

The file backend stores keys in `runtime/apikeys`.

---

## Shutting down

```lisp
(api:stop)     ; stop only the web API
(hab:shutdown) ; stop the entire Chipi instance
```

---

© 2024 Chipi contributors – Licensed under the Apache License 2.0
