# Chipi-Web

Chipi-Web ist eine optionale REST/JSON-Schicht für das Automatisierungs-Framework
**Chipi**.  Sie erlaubt das Auslesen und Aktualisieren von Items per HTTP
und nutzt Scope-basierte API-Keys für die Authentifizierung.

## Voraussetzungen
* SBCL / CCL / ABCL (getestet mit SBCL 2.x)
* Quicklisp
* Die ASDF-Systeme `chipi` **und** `chipi-web`

```lisp
;; Quicklisp-Installation
(ql:quickload :chipi-web)
```

## Schnellstart (REPL)

```lisp
(hab:defconfig "chipi"
  ;; Chipi-Web spezifische Initialisierung
  (api-env:init
    :apikey-store   apikey-store:*memory-backend*   ; oder (apikey-store:make-simple-file-backend)
    :apikey-lifetime (ltd:duration :day 100))

  ;; HTTP-API auf Port 8765 starten
  (api:start))
```

### Beispiel-Items definieren

```lisp
(defitem 'lamp   "Wohnzimmerlampe" 'boolean :initial-value 'item:false)
(defitem 'temp   "Temperatur"      'float   :initial-value 21.5)
```

## API-Keys erzeugen

```lisp
(defparameter *my-key*
  (apikey-store:create-apikey :access-rights '(:read :update)))
```

## REST-Schnittstelle

Die gesamte Spezifikation findet sich in `chipi-web-api.yaml`
(OpenAPI 3.0).

| Operation                 | HTTP Methode | Rechte |
|---------------------------|--------------|--------|
| `/items`                  | `GET`        | `:read` |
| `/items/{itemName}`       | `GET`        | `:read` |
| `/items/{itemName}`       | `POST`       | `:update` |

**Header:** `X-Api-Key: <dein-Key>`

### Beispiele (curl)

```bash
# Alle Items auflisten
curl -H "X-Api-Key: $MY_KEY" http://localhost:8765/items

# Einzelnes Item abfragen
curl -H "X-Api-Key: $MY_KEY" http://localhost:8765/items/lamp

# Item aktualisieren
curl -X POST -H "X-Api-Key: $MY_KEY" -H "Content-Type: application/json" \
     -d '{"value": true}' http://localhost:8765/items/lamp
```

## Rechte-Modell

* `:read`   – nur lesen  
* `:update` – Werte setzen  
* `:delete` – (reserviert)  
* `:admin`  – Vollzugriff

Der höchste angeforderte Scope muss ≤ höchstem Scope des Keys sein.

## API-Key-Persistenz

```lisp
(api-env:init
  :apikey-store (apikey-store:make-simple-file-backend))
```

Der File-Backend speichert Keys in `runtime/apikeys`.

## Server stoppen

```lisp
(api:stop)       ; nur Web-API
(hab:shutdown)   ; komplette Chipi-Instanz
```

## Lizenz
MIT – siehe Haupt-README.
