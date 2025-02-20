(defpackage :chipi.persistence
  (:use :cl)
  (:nicknames :persp)
  (:import-from #:act
                #:actor)
  (:export #:persistence
           #:base-map-persistence
           #:base-historic-persistence
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
           #:absolute-range
           #:make-relative-range
           #:make-absolute-range
           ;; range accessors
           #:seconds
           #:minutes
           #:hours
           #:days
           #:end-ts
           #:start-ts))

(in-package :chipi.persistence)

(defclass persistence (actor) ()
  (:documentation "This persistence is the base class of persistences.
`persistences' are actual implementations of database/file/whatever backends that read/write data."))

(defclass base-historic-persistence (persistence) ()
  (:documentation "A persistence type that can load ranged values from the past based on time-frames."))

(defclass base-map-persistence (persistence) ()
  (:documentation "A persistence type that represents a single (usually the last persisted) item value."))


(defclass range () ())

(defclass relative-range (range)
  ((seconds :initarg :seconds :initform nil :accessor seconds)
   (minutes :initarg :minutes :initform nil :accessor minutes)
   (hours :initarg :hours :initform nil :accessor hours)
   (days :initarg :days :initform nil :accessor days)
   (end-ts :initarg :end-ts :initform nil :accessor end-ts
           :documentation "End universal timestamp. In case ommited `now' is used.
Other values (seconds, minutes, etc.) are substracted."))
  (:documentation "A relative range is a range that is relative to an end boundary timestamp."))

(defclass absolute-range (range)
  ((start-ts :initarg :start-ts :initform nil :accessor start-ts)
   (end-ts :initarg :end-ts :initform nil :accessor end-ts))
  (:documentation "An absolute range by specifying start and end universal time (stamp)."))

(defstruct persisted-item
  (value nil)
  (timestamp nil))

(defgeneric initialize (persistence)
  (:documentation "Initializes the persistence."))

(defgeneric shutdown (persistence)
  (:documentation "Shuts down the persistence."))

(defgeneric persist (persistence item)
  (:documentation "Stores the item value.
The concrete persistence is responsible to store all the data that is also expected to be retrieved later.

Note that the timestamp of the item when last changed is persisted,
which may differ of the time when persisting.
Depends on the `delay' setting of the persistence when applied on the item."))

(defgeneric retrieve (base-map-persistence item)
  (:documentation "Fetches the last value of an item from the persistence as `persisted-item'.
The caller of this method is handling error conditions, so you don't have to necessarily.
But you can return more specific '(:error . error-message or condition)."))

(defgeneric retrieve-range (base-historic-persistence item range)
  (:documentation "Fetches a range of values of an item from the persistence as a list of `persisted-item's.
The caller of this method is handling error conditions, so you don't have to necessarily.
But you can return more specific '(:error . error-message or condition)."))
