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
           #:fetch))

(in-package :cl-hab.persistence)

(defclass persistence (actor) ()
  (:documentation "This persistence is the base class of sub-persistences.
There may be different kinds of persistence with different storages."))

(defgeneric initialize (persistence)
  (:documentation "Initializes the persistence."))

(defgeneric shutdown (persistence)
  (:documentation "Shuts down the persistence."))

(defgeneric persist (persistence item value)
  (:documentation "Stores the value of an item to file."))

(defgeneric retrieve (persistence item)
  (:documentation "Fetches the value of an item from file."))

