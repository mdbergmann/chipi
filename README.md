# Chipi Web UI

Dies ist eine Web-Oberfläche für das Chipi-System zur Anzeige und Steuerung von Items über eine REST-API.

## Features

- Anzeige aller Items mit Name, Label, Wert, Typ-Hinweis und Zeitstempel
- Werteänderung (inkl. Toggle für boolesche Werte)
- API-Key-Authentifizierung
- Fehlerbehandlung für fehlende oder unzureichende Rechte
- Responsive UI mit Lit und TypeScript

## Entwicklung

### Voraussetzungen

- Node.js (empfohlen: >=18)
- npm

### Starten der Entwicklungsumgebung

```sh
cd ui-ts
npm install
npm run dev
```

Die UI ist dann unter [http://localhost:5173](http://localhost:5173) erreichbar.  
Die API wird per Proxy an [http://localhost:8765](http://localhost:8765) weitergeleitet.

### Build für Produktion

```sh
npm run build
```

Das gebaute Frontend liegt dann im `ui-ts/dist`-Verzeichnis.

## API

Die REST-API ist in [`chipi-web-api.yaml`](chipi-web-api.yaml) dokumentiert (OpenAPI 3.0).

- Alle Requests benötigen einen gültigen API-Key im Header `X-Api-Key`.
- Rechte werden über Scopes (`read`, `update`) gesteuert.

## Projektstruktur

- `ui-ts/` – Quellcode der Web-Oberfläche (TypeScript, Lit)
- `src/` – Backend-Quellcode (Common Lisp)
- `chipi-web-api.yaml` – OpenAPI-Spezifikation der REST-API

## Lizenz

MIT License
