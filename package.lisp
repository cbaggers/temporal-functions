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
           :repeat
           :whilst))

(defpackage :tiny-time-manager
  (:use :cl :temporal-functions)
  (:nicknames :ttm)
  (:export :update :add :release :clean))
