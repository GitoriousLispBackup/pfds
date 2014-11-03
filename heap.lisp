;;; heap.lisp --- Heap implementation

;;;; Heaps

;;; A heap, also called a "priority queue", is a data structure used
;;; when you need to efficiently access the minimum element.
;;;
;;; This file implements one heap data structure: Leftist Heap
;;;
;;; The generic functions implemented by heaps are:
;;;
;;; + delete-min
;;; + empty-p
;;; + find-min
;;; + insert
;;; + join
;;;
;;; When creating a new heap, you need to specify the predicate
;;; function, reponsible of ordering the elements.

(in-package #:pfds)


;;;; Leftist Heap

;;; To make a leftist heap, use `make-leftist-heap' or `leftist-heap'.

(defvar empty-leftist-heap nil)

(defun leftist-heap-rank (heap)
  (if (eq heap empty-leftist-heap)
      0
      (destructuring-bind (r . rest) heap
        (declare (ignore rest))
        r)))

(defun leftist-heap-empty-p (heap)
  (null heap))

(defun leftist-heap-make-heap (x a b)
  (let ((rank-a (leftist-heap-rank a))
        (rank-b (leftist-heap-rank b)))
    (if (>= rank-a rank-b)
        (list (1+ rank-b) x a b)
        (list (1+ rank-a) x b a))))

(defun leftist-heap-merge (heap1 heap2 predicate)
  (cond ((null heap1) heap2)
        ((null heap2) heap1)
        (t
         (destructuring-bind (_ x a1 b1) heap1
           (declare (ignore _))
           (destructuring-bind (_ y a2 b2) heap2
             (declare (ignore _))
             (if (funcall predicate x y)
                 (leftist-heap-make-heap x a1
                                         (leftist-heap-merge b1
                                                             heap2
                                                             predicate))
                 (leftist-heap-make-heap y a2
                                         (leftist-heap-merge heap1
                                                             b2
                                                             predicate))))))))

(defun leftist-heap-insert (x heap predicate)
  (leftist-heap-merge (list 1 x empty-leftist-heap empty-leftist-heap)
                      heap
                      predicate))

(defun leftist-heap-find-min (heap)
  (if (eq heap empty-leftist-heap)
      (error 'empty)
      (destructuring-bind (_ x . rest) heap
        (declare (ignore _ rest))
        x)))

(defun leftist-heap-delete-min (heap predicate)
  (if (eq heap empty-leftist-heap)
      (error 'empty)
      (destructuring-bind (_ x a b) heap
        (declare (ignore _ x))
        (leftist-heap-merge a b predicate))))

(defstruct (leftist-heap
             (:constructor %make-leftist-heap)
             (:print-object print-leftist-heap))
  (predicate #'< :read-only t)
  (heap empty-leftist-heap :read-only t))

(defun print-leftist-heap (heap stream)
  (format stream "#<LEFTIST-HEAP~{ ~a~}>"
          (do ((list nil (cons (find-min heap) list))
               (heap heap (delete-min heap)))
              ((empty-p heap) (nreverse list)))))

(defun leftist-heap (predicate &rest xs)
  (do ((heap empty-leftist-heap (leftist-heap-insert (first xs) heap predicate))
       (xs xs (rest xs)))
      ((null xs) (%make-leftist-heap :predicate predicate
                                     :heap heap))))

(defun make-leftist-heap (size predicate &optional (initial-element t))
  (declare (type unsigned-byte size))
  (do ((heap empty-leftist-heap (leftist-heap-insert initial-element
                                                     heap
                                                     predicate))
       (i size (1- i)))
      ((zerop i) (%make-leftist-heap :predicate predicate
                                     :heap heap))))

(defmethod insert (x (heap leftist-heap))
  (let ((predicate (leftist-heap-predicate heap)))
    (%make-leftist-heap
     :predicate predicate
     :heap (leftist-heap-insert x
                                (leftist-heap-heap heap)
                                predicate))))

(defmethod find-min ((heap leftist-heap))
  (leftist-heap-find-min (leftist-heap-heap heap)))

(defmethod delete-min ((heap leftist-heap))
  (let ((predicate (leftist-heap-predicate heap)))
    (%make-leftist-heap
     :predicate predicate
     :heap (leftist-heap-delete-min (leftist-heap-heap heap)
                                    predicate))))

(defmethod empty-p ((heap leftist-heap))
  (leftist-heap-empty-p (leftist-heap-heap heap)))

(defmethod join ((heap1 leftist-heap) (heap2 leftist-heap))
  "Joined leftist-heap from HEAP1 and HEAP2.

Both heaps need to use the same predicate.  There is no check."
  (let ((predicate (leftist-heap-predicate heap1)))
    (%make-leftist-heap
     :predicate predicate
     :heap (leftist-heap-merge (leftist-heap-heap heap1)
                               (leftist-heap-heap heap2)
                               predicate))))
