;;; pfds.lisp --- Generic functions

(in-package #:pfds)

(defgeneric empty-p (collection)
  (:documentation "Is COLLECTION empty?"))

(defgeneric size (collection)
  (:documentation "The size of the collection.

How many elements this collection is holding."))

(defgeneric insert (x collection)
  (:documentation "Construct a new collection with X."))

(defgeneric head (collection)
  (:documentation "The head (first) element of COLLECTION."))

(defgeneric tail (collection)
  (:documentation "COLLECTION without its head."))

(defgeneric lookup (index collection)
  (:documentation "Element of COLLECTION at INDEX."))

(defgeneric update (index new-value collection)
  (:documentation "COLLECTION with index INDEX holding NEW-VALUE."))

(defgeneric drop (k collection)
  (:documentation "COLLECTION without the first K elements."))

;;; Admittedly, I wish I could call `con', `cons'.  Even though `cons'
;;; makes pairs in lisp, it is a common word used in functional
;;; circles to denote "CONStruction", not necessarily about pairs.
(defgeneric con (x collection)
  (:documentation "Collection with X to the left of COLLECTION."))

;;; To maintain consistency, `snoc', which is the inverse of `cons',
;;; is called `noc', which is the inverse of `con'.
(defgeneric noc (collection x)
  (:documentation "Collection with X to the right of COLLECTION."))

(defgeneric join (collection1 collection2)
  (:documentation "Join two collections together."))

(defgeneric find-min (collection)
  (:documentation "Find the minimum element in COLLECTION."))

(defgeneric delete-min (collection)
  (:documentation "COLLECTION without its minimum element."))
