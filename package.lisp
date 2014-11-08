;;;; package.lisp

(defpackage #:temporal-functions
  (:use #:cl #:fn_)
  (:export :def-time-units
           :milliseconds
           :seconds
           :minutes
           :hours

           :tlambda 
           :tdefun
           :before
           :after
           :between
           :each
           :then
           :repeat
           :whilst
           :%progress%
           :signal-expired
           :expiredp
           :expiredp+))

(defpackage :tiny-time-manager
  (:use :cl :temporal-functions)
  (:nicknames :ttm)
  (:export :update :add :release :clean))
