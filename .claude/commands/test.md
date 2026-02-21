---
description: Run Chipi tests (core, api, or all)
argument-hint: [core|api|all]
allowed-tools: Bash
---

Run the Chipi test suites via SBCL.

Based on `$ARGUMENTS`:

- **No argument or `core`**: Run core tests only:
  ```
  sbcl --noinform --non-interactive --eval '(asdf:test-system "chipi")'
  ```

- **`api`**: Run API tests only:
  ```
  sbcl --noinform --non-interactive --eval '(asdf:test-system "chipi-api")'
  ```

- **`all`**: Run both core and API tests:
  ```
  sbcl --noinform --non-interactive --eval '(progn (asdf:test-system "chipi") (asdf:test-system "chipi-api"))'
  ```

Show the tail end of the output focusing on the pass/fail summary. Report results concisely.
