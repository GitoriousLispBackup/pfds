;;; pfds.asd --- Defition of the PFDS ASDF system

(in-package #:asdf)

(defsystem #:pfds
  :description "Pure functional data structures."
  :version "0.0.1"
  :author "Diogo F. S. Ramos <dfsr@riseup.net>"
  :licence "LGPL3"
  :serial t
  :components ((:file "packages")
               (:file "pfds")
               (:file "delay")
               (:file "random-access-list")
               (:file "queue")
               (:file "heap")))
