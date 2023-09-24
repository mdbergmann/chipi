(defpackage :cl-hab.influx-persistence
  (:use :cl :cl-hab.persistence)
  (:nicknames :influx-persistence)
  (:import-from #:persp
                #:persistence)
  (:export #:influx-persistence
           #:make-influx-persistence)
  )

(in-package :cl-hab.influx-persistence)

(defclass influx-persistence (persistence)
  ())

(defun make-influx-persistence (id &key
                                     base-url
                                     token
                                     org
                                     bucket
                                     (precision "s"))
  (persp::make-persistence id
                           :type 'influx-persistence))

(defmethod initialize ((persistence influx-persistence))
  (log:info "Initializing persistence: ~a" persistence))

(defmethod shutdown ((persistence influx-persistence))
  (log:info "Shutting down persistence: ~a" persistence))

(defmethod persist ((persistence influx-persistence) item)
  (log:debug "Persisting, item: ~a" item)
  )
