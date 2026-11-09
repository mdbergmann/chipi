(in-package :chipi.binding.matter)

;; One Sento actor per Matter role, on a thread of its own (`:pinned'),
;; owning one cl-matter exchange manager.
;;
;; cl-matter is single-threaded and polled: every call that touches
;; sessions, exchanges or the served data model has to run on the thread
;; that polls the manager.  Here that thread is the actor's, and the actor
;; polls itself: a `(:poll)' message runs one turn of the manager and queues
;; the next `(:poll)' behind whatever arrived meanwhile, so a message to the
;; actor waits at most one poll timeout and runs in order with the
;; datagrams.  Everything cl-matter calls back -- a cluster's command
;; handler, a subscription report -- therefore runs inside receive, in actor
;; context, and from there the way out is a tell to an item
;; (`item:set-value'), which never blocks.
;;
;; Messages:
;;   (:poll)          one turn of the manager, then again
;;   (:call . thunk)  run THUNK in actor context; the reply is its value, or
;;                    the condition it signalled -- `matter-actor-call'
;;   (:run . thunk)   run THUNK in actor context, nothing comes back --
;;                    `matter-actor-run'
;;   (:stop)          end the loop, run the on-stop function, close the
;;                    manager -- `matter-actor-stop'

(defparameter *poll-timeout* 0.05d0
  "Seconds one poll turn waits for a datagram: the longest a message to a Matter actor waits.")

(defparameter *call-timeout* 10
  "Seconds `matter-actor-call' waits for the actor's answer.")

(defstruct (actor-state (:constructor %make-actor-state))
  "What a Matter actor holds: the exchange MANAGER it polls, whether its
loop is still RUNNING, and ON-STOP, a function of no arguments run in actor
context before the manager is closed."
  manager
  (running t)
  on-stop)

(defun %actor-name ()
  (act-cell:name act:*self*))

(defun %actor-receive (msg)
  (let ((state act:*state*))
    (case (car msg)
      (:poll
       (when (actor-state-running state)
         (handler-case
             (matter:exchange-manager-poll (actor-state-manager state)
                                           :timeout *poll-timeout*)
           (error (c)
             ;; one bad datagram must not end the actor
             (log:warn "Matter actor ~a: error in poll: ~a" (%actor-name) c)))
         (act:! act:*self* '(:poll))))
      (:call
       (act:reply (handler-case (funcall (cdr msg))
                    (error (c) c))))
      (:run
       (handler-case (funcall (cdr msg))
         (error (c)
           (log:warn "Matter actor ~a: ~a" (%actor-name) c))))
      (:stop
       (setf (actor-state-running state) nil)
       (handler-case
           (progn
             (when (actor-state-on-stop state)
               (funcall (actor-state-on-stop state)))
             (matter:exchange-manager-close (actor-state-manager state)))
         (error (c)
           (log:warn "Matter actor ~a: error on stop: ~a" (%actor-name) c)))
       (act:reply t))
      (t
       (log:warn "Matter actor ~a: unknown message: ~a" (%actor-name) msg)))))

(defun make-matter-actor (name manager &key on-stop)
  "Creates the actor `NAME' in chipi's item actor system, on a thread of its own, and starts it polling `MANAGER', a cl-matter exchange manager.
`ON-STOP', a function of no arguments, is run in actor context by `matter-actor-stop' before the manager is closed -- to stop advertising, write a state file.
The actor owns the manager from here on: touch it only through `matter-actor-call' and `matter-actor-run'."
  (ac:actor-of (isys:ensure-isys)
               :name name
               :dispatcher :pinned
               :state (%make-actor-state :manager manager :on-stop on-stop)
               :receive #'%actor-receive
               :init (lambda (self)
                       (log:info "Matter actor ~a: polling" name)
                       (act:! self '(:poll)))))

(defun matter-actor-call (actor thunk &key (timeout *call-timeout*))
  "Runs `THUNK', a function of no arguments, in `ACTOR's context and returns its value, waiting up to `TIMEOUT' seconds.
A condition `THUNK' signals is signalled here; so is an answer that does not come in time.
Never call this from the actor's own context: the actor would wait for itself."
  (multiple-value-bind (result fut)
      (future:fawait (act:? actor (cons :call thunk)) :timeout timeout :sleep-time 0.005)
    (unless (future:complete-p fut)
      (error "Matter actor ~a did not answer within ~a seconds" (act-cell:name actor) timeout))
    (when (typep result 'condition)
      (error result))
    result))

(defun matter-actor-run (actor thunk)
  "Runs `THUNK', a function of no arguments, in `ACTOR's context without waiting; an error it signals is logged."
  (act:! actor (cons :run thunk))
  t)

(defun matter-actor-stop (actor &key (timeout *call-timeout*))
  "Ends `ACTOR's poll loop, runs its on-stop function and closes its manager -- all in actor context -- then stops the actor and waits for it."
  (future:fawait (act:? actor '(:stop)) :timeout timeout :sleep-time 0.005)
  (ac:stop (act:context actor) actor :wait t)
  t)
