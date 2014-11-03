;;; delay.lisp --- Implement delay functionality

(in-package #:pfds)


(defstruct delay (value nil) (function nil))

(defmacro delay (&rest body)
  "A computation that can be executed later by FORCE."
  `(make-delay :function (lambda () . ,body)))

(defun force (x)
  "Find the value of X, by computing if it is a delay."
  (cond ((not (delay-p x)) x)
        (t (when (delay-function x)
             (setf (delay-value x) (funcall (delay-function x))
                   (delay-function x) nil))
           (delay-value x))))


;;;; Partial Pipe
(defvar empty-ppipe nil)

(defun ppipe-first (list)
  (cond ((null list) (error 'empty))
        ((consp list) (first list))
        (t (ppipe-first (force list)))))

(defun ppipe-rest (list)
  (cond ((null list) (error 'empty))
        ((consp list) (rest list))
        (t (ppipe-rest (force list)))))

(defun append-ppipe (l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        ((consp l1)
         (cons (first l1) (delay (append-ppipe (rest l1) l2))))
        (t (append-ppipe (force l1) l2))))

(defun reverse-ppipe (list)
  (labels ((rev (list acc)
             (cond ((null list) acc)
                   ((consp list)
                    (rev (rest list) (cons (first list) acc)))
                   (t (rev (force list) acc)))))
    (rev list nil)))
