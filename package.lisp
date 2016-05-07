;;;; package.lisp

(defpackage #:temporal-functions
  (:use #:cl)
  (:export :def-time-units
           :milliseconds
           :seconds
           :minutes
           :hours

           :tlambda
	   :defun-t
           :before
           :after
           :between
           :each
           :then
           :repeat
	   :once
           :whilst
           :%progress%
           :signal-expired
           :expiredp
           :expiredp+
	   :local-reset
	   :skip-step

           :make-stepper))

(defpackage :tiny-time-manager
  (:use :cl :temporal-functions)
  (:nicknames :ttm)
  (:export :update :add :release :clean :make-tfunc-pool))
