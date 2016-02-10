(in-package #:temporal-functions)
(named-readtables:in-readtable fn:fn-reader)

(defvar *default-time-source* 'get-internal-real-time)




;; (then x y)
;; (then~0 x y (forever (signal-expired)))

;; (repeat x y)
;; (repeat~0 x y (reset))

(defmacro tlambda (args &body then-clauses)
  `(let* ((state (make-array 1 :element-type 'fixnum :initial-element 0))
	  (initialized nil)
	  (time-source #',*default-time-source*)
	  (last-called (funcall time-source)))
     (lambda ,args
       (tlambda-top-level
	(then ,@then-clauses)))))

(defmacro %sys-val () `(aref state 0))
(defmacro %current-step () `(aref state %state-offset))

(defconstant +step+ 0)
(defconstant +collect-state-info+ 1)
(defconstant +init+ 2)

(defmacro tlambda-top-level (body-form)
  `(labels ((%get-time ()
	      (funcall time-source))
	    (%time-delta ()
	      (let ((x (%get-time)))
		(prog1 (- x last-called)
		  (setf last-called x))))
	    (step (%time %state-offset %sys-call)
	      ,body-form)
	    (dispatch ()
	      (when (not initialized)
		(let ((new-state-size (step 0 0 +collect-state-info+)))
		  (setf state (make-array new-state-size :element-type 'fixnum
					  :initial-element 0))
		  (state 0 0 +init+))
		(setf initialized t))
	      (step (%time-delta) 0 0 +step+)))
     (dispatch)))

;; (tlambda ()
;;   (before 100 (print "hi"))
;;   (before 100 (print "there")))


(defmacro then (&body then-clauses)
  `(then~1
     ,@then-clauses
     (forever (signal-expired))))

(defmacro def-temporal-clause (name state-vars args)
  )

(defmacro then~1 (&body then-clauses)
  (let ((steps (loop :for i :below (length then-clauses) :collect
		  (intern (format nil "step-~a" i)))))
    `(macrolet ()
       (labels (,@(loop :for step :in steps :for clause :in then-clauses :collect
		     `(,step (%time %state-offset %sys-call)
			     ,clause))
		;; some preliminary number, replace this.
		;; (the 1+ is because of the step)
		(how-many-slots ()
		  (1+ ,(length steps)))

		  (init (init-time %state-offset)
		    (setf (%current-step) 0))

		  (dispatch (%time %state-offset %sys-call)
		    (case= (%current-step)
		      ,@(loop :for step :in steps :for i :from 0 :collect
			   `(,i (,step %time %state-offset %sys-call))))))
	 (case= %sys-call
	   (+step+ (dispatch %time %sys-call))
	   (+collect-state-info+ (+ (how-many-slots)
		      ,@(mapcar λ`(,_ 0 %state-offset %sys-call) steps)))
	   (+init+
	    (init %time %state-offset)
	    ,(reduce λ`(,_1 0 ,_ %sys-call) steps
		     :initial-value '(+ %state-offset (how-many-slots))))
	   (otherwise (error "invalid %sys-call bug")))))))


(defmacro case= (form &body cases)
  (let ((g (gensym "val")))
    (labels ((wrap-case (c) `((= ,g ,(first c)) ,@(rest c))))
      (let* ((cases-but1 (mapcar #'wrap-case (butlast cases)))
	     (last-case (car (last cases)))
	     (last-case (if (eq (car last-case) 'otherwise)
			    `(t ,@(rest last-case))
			    (wrap-case last-case)))
	     (cases (append cases-but1 (list last-case))))
	`(let ((,g ,form))
	   (cond ,@cases))))))
