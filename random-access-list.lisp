;;; random-access-list.lisp --- Random-Access List implementation

;;;; Random-Access List

;;; A random-access list is a data structure that supports array-like
;;; `lookup' and `update' functions as well list functions like `con'
;;; ("cons"), `tail' and `head'.
;;;
;;; This file implements:
;;;
;;; + Binary Random-Access List
;;;
;;; The generic functions implemented by random-access lists are:
;;;
;;; + con
;;; + empty-p
;;; + head
;;; + lookup
;;; + tail
;;; + update

(in-package #:pfds)


;;;; Binary Random-Access List

;;; To make a binary random-access list, use
;;; `make-binary-random-access-list' and `binary-random-access-list'.
;;;
;;; Beyond the normal random-access list functions, a binary
;;; random-access list also implements the following functions:
;;;
;;; + size
;;; + drop

(defvar empty-bral nil)
(defun bral-empty-p (bral) (eq bral empty-bral))

(defstruct (bral-leaf (:constructor bral-leaf (x))) x)
(defstruct (bral-node (:constructor bral-node (w t1 t2)))
  w t1 t2)
(defstruct (bral-zero (:constructor bral-zero)))
(defstruct (bral-one (:constructor bral-one (tree)))
  tree)

(defun bral-size (x)
  (if (bral-leaf-p x)
      1
      (bral-node-w x)))

(defun bral-link (t1 t2)
  (bral-node (+ (bral-size t1) (bral-size t2))
             t1 t2))

(defun bral-cons-tree (t1 list)
  (if (null list)
      (list (bral-one t1))
      (destructuring-bind (digit . ts) list
        (if (bral-zero-p digit)
            (cons (bral-one t1) ts)
            (let ((t2 (bral-one-tree digit)))
              (cons (bral-zero)
                    (bral-cons-tree (bral-link t1 t2) ts)))))))

(defun bral-uncons-tree (tree)
  (if (null tree)
      (error "empty")
      (destructuring-bind (digit . ts) tree
        (if (bral-zero-p digit)
            (multiple-value-bind (node tsl) (bral-uncons-tree ts)
              (values (bral-node-t1 node)
                      (cons (bral-one (bral-node-t2 node)) tsl)))
            (let ((tr (bral-one-tree digit)))
              (if (null ts)
                  (values tr nil)
                  (values tr (cons (bral-zero) ts))))))))

(defun bral-add-zeros (n digits)
  (if (zerop n)
      digits
      (cons (bral-zero) (bral-add-zeros (1- n) digits))))

(defun bral-drop-tree (k tree &optional digits)
  (cond ((bral-leaf-p tree)
         (cond ((= k 1)
                (if digits
                    (cons (bral-zero) digits)
                    digits))
               ((= k 0)
                (cons (bral-one tree) digits))
               (t (error "subscript"))))
        ((zerop k)
         (bral-add-zeros (log (bral-size tree) 2)
                         (cons (bral-one tree) digits)))
        (t
         (if (< k (floor (bral-size tree) 2))
             (if digits
                 (if (bral-one-p (first digits))
                     (if (= (bral-size (bral-one-tree (first digits)))
                            (bral-size tree))
                         (bral-drop-tree k
                                         (bral-node-t1 tree)
                                         (cons (bral-one (bral-node-t2 tree))
                                               digits))
                         (bral-drop-tree k
                                         (bral-node-t1 tree)
                                         (cons (bral-one (bral-node-t2 tree))
                                               (cons (bral-zero) digits))))
                     (bral-drop-tree k
                                     (bral-node-t1 tree)
                                     (cons (bral-one (bral-node-t2 tree))
                                           (cons (bral-zero) digits))))
                 (bral-drop-tree k
                                 (bral-node-t1 tree)
                                 (cons (bral-one (bral-node-t2 tree)) digits)))
             (if digits
                 (if (bral-one-p (first digits))
                     (if (= (bral-size (bral-one-tree (first digits)))
                            (bral-size tree))
                         (bral-drop-tree (- k (floor (bral-size tree) 2))
                                         (bral-node-t2 tree) digits)
                         (bral-drop-tree (- k (floor (bral-size tree) 2))
                                         (bral-node-t2 tree)
                                         (cons (bral-zero) digits)))
                     (bral-drop-tree (- k (floor (bral-size tree) 2))
                                     (bral-node-t2 tree)
                                     (cons (bral-zero) digits)))
                 (bral-drop-tree (- k (floor (bral-size tree) 2))
                                 (bral-node-t2 tree)
                                 digits))))))

(defun bral-drop (k bral)
  (cond ((zerop k)
         (cond ((null bral)
                bral)
               ((bral-zero-p (first bral))
                bral)
               (t (bral-drop-tree k
                                  (bral-one-tree (first bral))
                                  (rest bral)))))
        ((null bral) (error "subscript"))
        (t
         (destructuring-bind (digit . digits) bral
           (if (bral-zero-p digit)
               (bral-drop k digits)
               (let* ((tree (bral-one-tree digit))
                      (size (bral-size tree)))
                 (if (or (< k size) (= k size))
                     (bral-drop-tree k tree digits)
                     (bral-drop (- k size) digits))))))))

(defun bral-cons (x ts)
  (bral-cons-tree (bral-leaf x) ts))

(defun bral-head (ts)
  (bral-leaf-x (nth-value 0 (bral-uncons-tree ts))))

(defun bral-tail (ts)
  (nth-value 1 (bral-uncons-tree ts)))

(defun bral-lookup-tree (i tree)
  (if (bral-leaf-p tree)
      (if (zerop i)
          (bral-leaf-x tree)
          (error 'subscript))
      (let ((w (bral-node-w tree))
            (t1 (bral-node-t1 tree))
            (t2 (bral-node-t2 tree)))
        (if (< i (floor w 2))
            (bral-lookup-tree i t1)
            (bral-lookup-tree (- i (floor w 2)) t2)))))

(defun bral-lookup (i tree)
  (if (null tree)
      (error 'subscript)
      (destructuring-bind (x . ts) tree
        (if (bral-zero-p x)
            (bral-lookup i ts)
            (let* ((tt (bral-one-tree x))
                   (size (bral-size tt)))
              (if (< i size)
                  (bral-lookup-tree i tt)
                  (bral-lookup (- i size) ts)))))))

(defun bral-update-tree (i y x)
  (if (bral-leaf-p x)
      (if (zerop i)
          (bral-leaf y)
          (error 'subscript))
      (let ((w (bral-node-w x))
            (t1 (bral-node-t1 x))
            (t2 (bral-node-t2 x)))
        (if (< i (floor w 2))
            (bral-node w (bral-update-tree i y t1) t2)
            (bral-node w t1 (bral-update-tree (- i (floor w 2)) y t2))))))

(defun bral-update (i y list)
  (if (null list)
      (error 'subscript)
      (destructuring-bind (digit . ts) list
        (if (bral-zero-p digit)
            (cons (bral-zero) (bral-update i y ts))
            (let* ((tree (bral-one-tree digit))
                   (size (bral-size tree)))
              (if (< i size)
                  (cons (bral-one (bral-update-tree i y tree)) ts)
                  (cons digit (bral-update (- i size) y ts))))))))

(defun bral-length (bral)
  (reduce '+ (mapcar 'bral-one-tree (remove-if-not 'bral-one-p bral))
          :key 'bral-size
          :initial-value 0))

(defun make-enumerated-bral (size)
  (loop for x downfrom (1- size) to 0
     for b = (bral-cons x empty-bral) then (bral-cons x b)
     finally (return b)))

(defun make-bral-dropped (dropped size)
  (loop for x downfrom (1- size) to dropped
     for b = (bral-cons x empty-bral) then (bral-cons x b)
     finally (return b)))

(defstruct (binary-random-access-list
             (:constructor %make-bral)
             (:print-object print-binary-random-access-list))
  (bral empty-bral :read-only t))

(defun print-binary-random-access-list (bral stream)
  (format stream "#<BINARY-RANDOM-ACCESS-LIST~{ ~a~}>"
          (loop for i below (size bral)
             collect (lookup i bral))))

(defun make-binary-random-access-list (size &optional (initial-element t))
  (do ((b empty-bral (bral-cons initial-element b))
       (i size (1- i)))
      ((zerop i) (%make-bral :bral b))))

(defun list->bral (list)
  (reduce 'bral-cons
          list
          :initial-value empty-bral
          :from-end t))

(defun binary-random-access-list (&rest elements)
  (%make-bral :bral (list->bral elements)))

(defmethod empty-p ((bral binary-random-access-list))
  (bral-empty-p (binary-random-access-list-bral bral)))

(defmethod size ((bral binary-random-access-list))
  (bral-length (binary-random-access-list-bral bral)))

(defmethod con (element (bral binary-random-access-list))
  "Insert ELEMENT in front of BRAL."
  (%make-bral :bral (bral-cons element (binary-random-access-list-bral bral))))

(defmethod head ((bral binary-random-access-list))
  (bral-head (binary-random-access-list-bral bral)))

(defmethod tail ((bral binary-random-access-list))
  (%make-bral :bral (bral-tail (binary-random-access-list-bral bral))))

(defmethod lookup (index (bral binary-random-access-list))
  (check-type index (integer 0 *))
  (bral-lookup index (binary-random-access-list-bral bral)))

(defmethod update (index new-value (bral binary-random-access-list))
  (check-type index (integer 0 *))
  (%make-bral :bral (bral-update index new-value
                                 (binary-random-access-list-bral bral))))

(defmethod drop (k (bral binary-random-access-list))
  (check-type k (integer 0 *))
  (%make-bral :bral (bral-drop k (binary-random-access-list-bral bral))))
