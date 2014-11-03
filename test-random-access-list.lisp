;;; test-random-access-list.lisp --- Test Random-Access list

(in-package #:pfds)

;;;; Binary Random-access List
(defmacro generate-test-bral-drop (limit)
  `(check
     ,@(loop for size to limit
          append (loop for drop to size collect
                      `(equalp (bral-drop ,drop (make-enumerated-bral ,size))
                               (make-bral-dropped ,drop ,size))))))

(deftest test-bral-drop ()
  (generate-test-bral-drop 20))

(deftest test-bral ()
  (let ((x (bral-cons
            42
            (bral-cons 'quux
                       (bral-cons 'baz
                                  (bral-cons 'bar
                                             (bral-cons 'foo
                                                        empty-bral)))))))
    (check
      (= 42 (bral-head x))
      (eq (bral-head (bral-tail x)) 'quux)
      (eq (bral-head (bral-tail (bral-tail x))) 'baz)
      (eq (bral-lookup 4 x) 'foo)
      (eq (bral-lookup 3 x) 'bar)
      (eq (bral-lookup 1 (bral-cons 'corge (bral-tail x))) 'quux)
      (eq (bral-lookup 0 (bral-update 0 'corge x)) 'corge)
      (eq (bral-lookup 3 (bral-update 3 'corge x)) 'corge)
      (eq (bral-lookup 4 (bral-update 4 'corge x)) 'corge)
      (= (bral-length x) 5)
      (= (bral-length empty-bral) 0)
      (= (bral-length (bral-cons 42 empty-bral)) 1)
      ;; drop
      (null (bral-drop 0 empty-bral))
      (null (bral-drop 1 (make-enumerated-bral 1)))
      (null (bral-drop 2 (make-enumerated-bral 2)))
      (null (bral-drop 3 (make-enumerated-bral 3)))
      (null (bral-drop 4 (make-enumerated-bral 4)))
      (equalp (bral-drop 0 (make-enumerated-bral 1))
              (make-enumerated-bral 1))
      (equalp (bral-drop 0 (make-enumerated-bral 2))
              (make-enumerated-bral 2))
      (equalp (bral-drop 1 (make-enumerated-bral 2))
              (make-bral-dropped 1 2))
      (equalp (bral-drop 1 (make-enumerated-bral 3))
              (make-bral-dropped 1 3))
      (equalp (bral-drop 1 (make-enumerated-bral 4))
              (make-bral-dropped 1 4))
      (equalp (bral-drop 0 (make-enumerated-bral 4))
              (make-bral-dropped 0 4))
      (equalp (bral-drop 2 (make-enumerated-bral 4))
              (make-bral-dropped 2 4))
      )))

(deftest test-binary-numbers ()
  (test-bral)
  (test-bral-drop))
