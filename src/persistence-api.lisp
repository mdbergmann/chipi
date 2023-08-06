(defpackage :cl-hab.persistence
  (:use :cl)
  (:nicknames :persp)
  (:import-from #:act
                #:actor)
  (:export #:persistence
           ;; persistence implementations
           #:persist
           #:retrieve
           #:initialize
           #:shutdown
           ;; public interface
           #:destroy
           #:store
           #:fetch
           #:persisted-item
           #:make-persisted-item
           #:persisted-item-value
           #:persisted-item-timestamp))

(in-package :cl-hab.persistence)

(defclass persistence (actor) ()
  (:documentation "This persistence is the base class of sub-persistences.
There may be different kinds of persistence with different storages."))

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
  (:documentation "Fetches the attributes of an item from file as `persisted-item'."))
