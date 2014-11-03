;;; queue.lisp --- FIFO Queue implementation

;;;; Queues

;;; FIFO (First in, First out) queues are data structures which the
;;; first element added is the first element available.
;;;
;;; Two FIFO queues are implemented here:
;;;
;;; + Batched Queue
;;; + Banker's Queue
;;;
;;; The generic functions implemented by queues are:
;;;
;;; + empty-p
;;; + noc
;;; + head
;;; + tail

(in-package #:pfds)


;;;; Batched Queue

;;; To make a batched queue, use `make-batched-queue' or
;;; `batched-queue'.

(defvar empty-batched-queue '(nil . nil))

(defun batched-queue-empty-p (batched-queue)
  (destructuring-bind (f . r) batched-queue
    (declare (ignore r))
    (null f)))

(defun batched-queue-checkf (batched-queue)
  (destructuring-bind (f . r) batched-queue
    (if (null f)
        (cons (reverse r) nil)
        batched-queue)))

(defun batched-queue-snoc (queue x)
  (destructuring-bind (f . r) queue
    (batched-queue-checkf (cons f (cons x r)))))

(defun batched-queue-head (queue)
  (destructuring-bind (f . r) queue
    (declare (ignore r))
    (if (null f)
        (error 'queue-empty)
        (first f))))

(defun batched-queue-tail (queue)
  (destructuring-bind (f . r) queue
    (if (null f)
        (error 'queue-empty)
        (batched-queue-checkf (cons (rest f) r)))))

(defstruct (batched-queue
             (:constructor %make-batched-queue)
             (:print-object print-batched-queue))
  (bq empty-batched-queue :read-only t))

(defun batched-queue->list (queue)
  (do ((q (batched-queue-bq queue) (batched-queue-tail q))
       (xs nil (cons (batched-queue-head q) xs)))
      ((batched-queue-empty-p q) (nreverse xs))))

(defun print-batched-queue (queue stream)
  (format stream "#<BATCHED-QUEUE~{ ~a~}>" (batched-queue->list queue)))

(defun make-batched-queue (size &optional (initial-element t))
  (declare (type unsigned-byte size))
  (do ((i size (1- i))
       (q empty-batched-queue (batched-queue-snoc q initial-element)))
      ((zerop i) (%make-batched-queue :bq q))))

(defun batched-queue (&rest xs)
  (do ((q empty-batched-queue (batched-queue-snoc q (first xs)))
       (xs xs (rest xs)))
      ((null xs) (%make-batched-queue :bq q))))

(defmethod empty-p ((queue batched-queue))
  (batched-queue-empty-p (batched-queue-bq queue)))

(defmethod noc ((queue batched-queue) x)
  (%make-batched-queue
   :bq (batched-queue-snoc (batched-queue-bq queue) x)))

(defmethod head ((queue batched-queue))
  (batched-queue-head (batched-queue-bq queue)))

(defmethod tail ((queue batched-queue))
  (%make-batched-queue
   :bq (batched-queue-tail (batched-queue-bq queue))))


;;;; Bankers Queue

;;; To make a banker's queue, use `make-bankers-queue' or
;;; `bankers-queue'.
;;;
;;; A banker's queue uses delayed evaluation to achieve O(1) amortized
;;; time.
;;;
;;; Keep in mind that when printing the queue, the printer will force
;;; all the delayed computations.

(defvar empty-bankers-queue (list 0 empty-ppipe 0 empty-ppipe))

(defun bankers-queue-empty-p (queue)
  (destructuring-bind (lenf &rest rest) queue
    (declare (ignore rest))
    (zerop lenf)))

(defun bankers-queue-check (queue)
  (destructuring-bind (lenf f lenr r) queue
    (if (<= lenr lenf)
        queue
        (list (+ lenr lenf)
              (append-ppipe f (delay (reverse-ppipe r)))
              0
              empty-ppipe))))

(defun bankers-queue-snoc (queue x)
  (destructuring-bind (lenf f lenr r) queue
    (bankers-queue-check (list lenf
                               f
                               (+ lenr 1)
                               (cons x r)))))

(defun bankers-queue-head (queue)
  (destructuring-bind (lenf f lenr r) queue
    (declare (ignore lenr r))
    (if (zerop lenf)
        (error 'empty)
        (ppipe-first f))))

(defun bankers-queue-tail (queue)
  (destructuring-bind (lenf f lenr r) queue
    (if (zerop lenf)
        (error 'empty)
        (bankers-queue-check (list (- lenf 1) (ppipe-rest f) lenr r)))))

(defstruct (bankers-queue
             (:constructor %make-bankers-queue)
             (:print-object print-bankers-queue))
  (bq empty-bankers-queue :read-only t))

(defun print-bankers-queue (queue stream)
  (format stream "#<BANKERS-QUEUE~{ ~a~}>"
          (do ((list nil (cons (head queue) list))
               (queue queue (tail queue)))
              ((empty-p queue) (nreverse list)))))

(defun bankers-queue (&rest xs)
  (do ((q empty-bankers-queue (bankers-queue-snoc q (first xs)))
       (xs xs (rest xs)))
      ((null xs) (%make-bankers-queue :bq q))))

(defun make-bankers-queue (size &optional (initial-element t))
  (declare (type unsigned-byte size))
  (do ((i size (1- i))
       (q empty-bankers-queue (bankers-queue-snoc q initial-element)))
      ((zerop i) (%make-bankers-queue :bq q))))

(defmethod empty-p ((q bankers-queue))
  (bankers-queue-empty-p (bankers-queue-bq q)))

(defmethod noc ((q bankers-queue) x)
  (%make-bankers-queue
   :bq (bankers-queue-snoc (bankers-queue-bq q) x)))

(defmethod head ((q bankers-queue))
  (bankers-queue-head (bankers-queue-bq q)))

(defmethod tail ((q bankers-queue))
  (%make-bankers-queue
   :bq (bankers-queue-tail (bankers-queue-bq q))))
