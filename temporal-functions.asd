;;;; temporal-functions.asd

(asdf:defsystem #:temporal-functions
  :description "A means of creating functions that have an internal concept of time"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "2 Clause BSD"
  :serial t
  :depends-on (#:macroexpand-dammit #:fn)
  :components ((:file "package")
	       (:file "helpers")
               (:file "units")
               ;;(:file "temporal-functions")
	       (:file "core")
               (:file "tiny-time-manager")))
