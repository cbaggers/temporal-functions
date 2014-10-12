;;;; package.lisp

(defpackage #:temporal-functions
  (:use #:cl #:fn_)
  (:export :def-time-units
           :milliseconds
           :seconds
           :minutes
           :hours
           :before
           :after
           :between
           :each
           :whilst))

(defpackage :tiny-time-manager
  (:use :cl :cepl-utils :base-time)
  (:nicknames :ttm)
  (:export :update :add :release :clean))
