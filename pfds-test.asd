;;; pfds-test.asd --- Define a test system for PFDS

(in-package #:asdf)

(defsystem #:pfds-test
  :description "Test system for the pfds system."
  :depends-on (#:pfds)
  :components ((:file "test")
               (:file "test-pfds")
               (:file "test-random-access-list")))
