---
description: Start the Chipi web server with example-web.lisp
allowed-tools: Bash
---

Start the Chipi web server by loading chipi-ui and example-web.lisp in SBCL.

Run this command in the background:

```
sbcl --noinform --eval '(progn (asdf:load-system "chipi-ui") (load "example-web.lisp") (format t "~%SERVER STARTED on http://localhost:8080~%") (loop (sleep 60)))'
```

Tell the user the server is starting and will be available at http://localhost:8080 once loaded.
