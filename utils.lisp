
(in-package #:temporal-functions)

(defun symb (&rest parts)
  (intern (format nil "~{~a~}" parts)))

(defun last-elm (list)
  (car (last list)))
