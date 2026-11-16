---
id: CORE-TABLE-001
type: use-case
parent: CORE
title: Compound item values
status: implemented
change_class: breaking
actors: [Application, Resident]
emits: [ItemChanged]
consumes: [ValueReceived]
tags: [items, persistence, api, ui]
created: 2026-11-15
updated: 2026-11-16
---

## § Intent
Applications store and residents inspect multiple fields as one item value.

## § Domain Rules
- **DR-1**: A value is a complete snapshot of named scalar fields. Invalid snapshots do not change state.
- **DR-2**: Equal snapshots do not produce state changes. Caller mutations cannot change stored snapshots.
- **DR-3**: Restart recovery preserves the snapshot and its timestamp without publishing or storing it again.
- **DR-4**: History preserves field names, field values and observation times. Missing fields are not filled from older observations.
- **DR-5**: Residents can inspect current fields and chart a selected field. Missing numeric values appear as gaps.

## § Acceptance Criteria
```gherkin
Scenario: Receive a compound observation
  Given an item with a compound value
  When Application submits a valid snapshot
  Then all fields become visible together
Scenario: Reject an invalid observation
  When Application submits a nested field
  Then the item retains its previous value
Scenario: Recover and inspect history
  Given persisted observations
  When Application restarts and Resident opens history
  Then the last snapshot is restored and observations retain their own fields
```

## § Domain Model Touch
Aggregate: Item. Event: ItemChanged. Read models: current value, history, UI.

## § Constraints
- Existing scalar item behavior remains compatible.
- Tests use an isolated disposable database and preserve the smart-home stand.

## § Open Questions
None.

## § Decision Log
- Use the `table` hint for Common Lisp hash-table values, flat string keys, finite real numbers, strings and `item:true`/`item:false`.
- InfluxDB stores all table numbers as floats. Simple persistence preserves integer/float distinctions.
- Empty snapshots are persisted to files but skipped in InfluxDB.
- No editor, MQTT binding or timestamp precision change.

## § Tech Spec
Extend JSON field conversions, file recovery, InfluxDB serialization and typed CSV parsing.
Group historic fields by timestamp, preserve key case, and aggregate each numeric field independently.
Add `:field` to `page:chart`; apply transforms after field selection for history and live updates.

## § Test Plan
- **TC-0**: Run existing database tests twice on fresh disposable instances before table implementation.
- **TC-1 → DR-1/2**: Reject invalid fields and invalid redefinitions; preserve the original actor, timestamp and memberships.
  Copy mutable input/output strings; compare equal snapshots without events; retain `:push :always`.
- **TC-2 → DR-3**: Persist/reload mixed and empty snapshots; retain timestamps without events, writes or binding pushes.
- **TC-3 → DR-4**: Write two observations with removed fields and integer-to-float readings.
  Read latest/ranges; verify all scalar field types, key case, CSV quoting and all six aggregates.
- **TC-4 → DR-4**: Capture a state before an update; ensure its fields and timestamp remain one immutable observation.
- **TC-5 → DR-5**: Render current/empty fields safely; replace fields on updates; select the same chart field for history/live data.
  Distinguish missing values (`null`) from other-series alignment (`undefined`) and preserve scalar chart behavior.
- **TC-6 → DR-1/5**: Verify API GET/POST/SSE; invalid POST returns HTTP 400 without mutation.
- **TC-7**: Run the full core/API/UI suites and publish all seven sensor fields through isolated MQTT and InfluxDB.


## § Implementation Notes
Work in an isolated checkout of `influx-hash-tables`. Do not edit installed `.qlot` sources.

Baseline: 46/46 existing database checks passed twice before table implementation.
InfluxDB integration: 104/104; standalone serializer/CSV checks: 32/32.
The isolated stand acceptance check matched all seven numeric fields through MQTT.
Only the disposable stand copy admitted the ephemeral loopback database port.

InfluxDB transport rejects empty field names, exact lowercase `time`, names ending
in backslash, and CR/LF in names/string values before HTTP. File/JSON accepts those
strings. Latest table retrieval currently scans its history to select a complete
latest observation; seconds precision can merge points within one second.

Browser verification was unavailable because the browser access host had no
surfaces. Offline CLOG rendering checks and the local UI HTTP startup passed.


Final verification after correctness review:
- Full suite: core 629/629, API 226/226, UI 799/799 (1,654 checks).
- No-database command: selected core/serializer 245/245, API 226/226, UI 799/799.
- Final isolated MQTT acceptance: all seven numeric fields matched.
- Mallet introduced no actionable findings relative to the original branch.
  Its unused-local-nicknames report for the new unit file is a false positive:
  the `influx::` references use that nickname throughout the file.
- `git diff --check` and Python AST parsing passed. Disposable containers and volumes were removed.

Type hint update (2026-11-16): use `table` in definitions and `TABLE` in JSON metadata.
The stored value remains a Common Lisp hash-table. Recognition is independent of the caller package.
