;;;; temporal-functions.asd

(asdf:defsystem #:temporal-functions
  :serial t
  :description "A means of creating functions that have an internal concept of time"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "2 Clause BSD"
  :depends-on (#:macroexpand-dammit
               #:alexandria
               #:fn_)
  :components ((:file "package")
               (:file "utils")
               (:file "temporal-functions")))

