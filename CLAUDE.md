# Chipi - Common Lisp Home Automation Framework

## Project Overview

Chipi is a lightweight home automation framework in Common Lisp, an alternative to openHAB. It manages **items** (sensors, switches), **itemgroups** (containers), **bindings** (external system integration), **persistences** (storage), and **rules** (event/cron-triggered automation). It uses actor-based concurrency (Sento) for thread safety and async messaging.

## Architecture

- **Core** (`chipi`): Items, itemgroups, bindings, persistence, rules, environment, timers/cron
- **API** (`chipi-api`): REST/JSON via Hunchentoot + Snooze, SSE, scope-based auth with API keys
- **UI** (`chipi-ui`): Web UI via CLOG (WebSockets)
- **Bindings** (plugins): e.g. `binding-knx` for KNX bus protocol

Key entry point: `hab.lisp` provides DSL macros (`defconfig`, `defitem`, `defitemgroup`, `defrule`, `defpersistence`, `binding`).

## Code Style

Write idiomatic Common Lisp:

- **Naming**: `kebab-case` for functions/variables. `%` prefix for internal helpers (e.g. `%parse-frequency`). `-p` suffix for predicates (e.g. `true-p`, `call-push-p`). Earmuffs for specials (`*items*`).
- **Packages**: `:chipi.<subsystem>` with short nicknames (`:item`, `:hab`, `:rule`, etc.). Use `:import-from` and `:local-nicknames` for clarity.
- **Classes vs structs**: `defclass` for complex objects with behavior (item, rule, binding). `defstruct` for simple data (item-state, item-persistence).
- **Macros**: Use `with-gensyms` from Alexandria for hygiene. DSL macros are the primary configuration interface.
- **Error handling**: `handler-case` for recovery, `ignore-errors` for fire-and-forget cleanup, `unwind-protect` for guaranteed cleanup.
- **Concurrency**: Actor messages via `!` (tell) and `?` (ask). Futures with `fmap`, `fcompleted`, `fresult`. Never access actor state outside actor context.
- **Logging**: `log4cl` — use `log:info`, `log:debug`, `log:warn` with format strings.
- **Formatting**: 2-space indentation. Keep lines reasonable length. Docstrings on exported functions, classes, and macros.

## Testing

### Framework

FiveAM (`fiveam`) with `cl-mock` for mocking. Tests tightly cover production code — every production change must have corresponding test changes.

### Running Tests

Use the `/test` skill to run tests. Available options: core tests, API tests, or all tests.

### Test Conventions

- **Package**: `:chipi.<module>-test`, using `:cl :fiveam :cl-mock`
- **Suites**: `def-suite <module>-tests :in chipi.tests:test-suite`
- **Naming**: `test <function>--<scenario>` with double-dash separating function from scenario
- **Fixtures**: `def-fixture` with `unwind-protect` for setup/teardown (always call `envi:shutdown-env` and clean temp dirs)
- **Assertions**: `is`, `is-true`, `is-false` from FiveAM
- **Async**: Use `await-cond <timeout> <form>` for waiting on futures/async results
- **Mocking**: `with-mocks ()` + `answer <function> <result>`, verify with `(invocations '<function>)`

### Test Pattern Example

```lisp
(def-fixture init-destroy-env ()
  (unwind-protect
       (progn (&body))
    (progn
      (envi:shutdown-env)
      (delete-folder #P"/tmp/chipi"))))

(test make-item--with-initial-value
  (with-fixture init-destroy-env ()
    (let ((item (make-item 'my-item :label "label" :initial-value 12345)))
      (is-true item)
      (is (eq (item:item-state-value (item:get-item-stateq item)) 12345)))))
```

## Dependencies

- **sento** (>=3.4.2): Actor framework
- **alexandria**: Utility library
- **com.inuoe.jzon**: JSON
- **hunchentoot** + **snooze**: HTTP server + REST routing (API)
- **clog**: Web UI framework (UI)
- **fiveam** + **cl-mock**: Testing
- Package manager: **OCICL**

## Key Files

| File | Purpose |
|------|---------|
| `src/hab.lisp` | DSL macros, main entry point |
| `src/item.lisp` | Item actor class and operations |
| `src/itemgroup.lisp` | Itemgroup container |
| `src/rule.lisp` | Rules engine |
| `src/binding-api.lisp` | Binding class definitions |
| `src/persistence-api.lisp` | Persistence interface |
| `src/env.lisp` | Environment init/shutdown |
| `src/api/api.lisp` | HTTP server setup |
| `test/all-tests.lisp` | Test suite registry |
