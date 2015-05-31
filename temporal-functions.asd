;;;; temporal-functions.asd

(asdf:defsystem #:temporal-functions
  :description "A means of creating functions that have an internal concept of time"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "2 Clause BSD"
  :serial t
  :depends-on (#:macroexpand-dammit)
  :components ((:file "package")
               (:file "units")
               (:file "temporal-functions")
               (:file "tiny-time-manager")))

