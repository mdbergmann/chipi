(defpackage :cl-hab.persistence
  (:use :cl)
  (:nicknames :persp)
  (:import-from #:act
                #:actor)
  (:export #:persistence
           ;; persistence implementations
           #:persist
           #:retrieve
           #:retrieve-range
           #:initialize
           #:shutdown
           ;; public interface
           #:destroy
           #:store
           #:fetch
           #:persisted-item
           #:make-persisted-item
           #:persisted-item-value
           #:persisted-item-timestamp
           #:range
           #:relative-range
           #:make-relative-range
           ;; range accessors
           #:seconds
           #:minutes
           #:hours
           #:days))

(in-package :cl-hab.persistence)

(defclass persistence (actor) ()
  (:documentation "This persistence is the base class of sub-persistences.
There may be different kinds of persistence with different storages."))

(defclass range () ())
(defclass relative-range (range)
  ((seconds :initarg :seconds :initform nil :accessor seconds)
   (minutes :initarg :minutes :initform nil :accessor minutes)
   (hours :initarg :hours :initform nil :accessor hours)
   (days :initarg :days :initform nil :accessor days))
  (:documentation "A relative range is a range that is relative to the current time."))

(defstruct persisted-item
  (value nil)
  (timestamp nil))

(defgeneric initialize (persistence)
  (:documentation "Initializes the persistence."))

(defgeneric shutdown (persistence)
  (:documentation "Shuts down the persistence."))

(defgeneric persist (persistence item)
  (:documentation "Stores the item to file.
The persistence is responsible to store all the data that is also expected to be retrieved later."))

(defgeneric retrieve (persistence item)
  (:documentation "Fetches the last value of an item from the persistence as `persisted-item'.
In case of error on fetch you should return '(:error . error-message or condition),
in which case the item value will not be set."))

(defgeneric retrieve-range (persistence item range)
  (:documentation "Fetches a range of values of an item from the persistence as a list of `persisted-item's."))
