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

(defconstant +step+ 0)
(defconstant +collect-state-info+ 1)
(defconstant +init+ 2)
(defconstant +local-init+ 3)

(defmacro tlambda-top-level (body-form)
  `(labels ((%get-time ()
	      (funcall time-source))
	    (%time-delta ()
	      (let ((x (%get-time)))
		(prog1 (- x last-called)
		  (setf last-called x))))
	    (reset ()
	      (print "reset not implemented"))
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

(tlambda ()
  (before 100 (print "hi"))
  (before 100 (print "there")))

(defmacro then (&body then-clauses)
  `(then~1
     ,@then-clauses
     (forever (signal-expired))))

(defmacro repeat (&body repeat-clauses)
  `(then~1
     ,@repeat-clauses
     (reset)))

(def-stepping-temporal-clause then~1 then-clauses
  (x y))

(defmacro def-stepping-temporal-clause (name clauses-name state-vars
					&key local-init)
  `(defmacro ,name (&body ,clauses-name)
     (let ((steps (loop :for i :below (length ,clauses-name) :collect
		     (intern (format nil "step-~a" i))))
	   (step-offsets
	    (loop :for i :below (length ,clauses-name) :collect
	       (intern (format nil "step-offset-~a" i))))
	   (local-init ,local-init))
       `(macrolet ((%current-step (aref state %state-offset))
		   ,@(loop :for o :in step-offsets :for i :from 0 :collect
			`(,o (aref state (+ %state-offset ,i))))
		   ,@(loop :for v :in ',state-vars
			:for i :from ,(length state-vars) :collect
			`(,v (aref state (+ %state-offset ,i)))))
	  (labels (,@(loop :for step :in steps :for clause :in ,clauses-name :collect
			`(,step (%time %state-offset %sys-call)
				,clause))

		   (next (remaining-time %state-offset)
		     (incf %current-step)
		     (dispatch remaining-time +local-init+))

		     ;; some preliminary number, replace this.
		     ;; (the 1+ is because of the step)
		     (how-many-slots ()
		       (1+ (+ ,(length steps) ,,(length state-vars))))

		     ;; get a list of the number of state slots each step uses
		     (slot-counts-from-steps ()
		       (list ,@(mapcar λ`(,_ 0 0 +collect-state-info+) steps)))

		     ;; tfunc-wide init
		     (init (%state-offset)
		       ;; cache offsets for each step
		       (let ((sizes (slot-counts-from-steps))
			     (own (how-many-slots))
			     (rolling-total (rolling-add sizes)))
			 ,@(loop :for o :in step-offsets :for i :from 0 :collect
			      `(setf ,o (+ %state-offset
					   own
					   (nth ,i rolling-total))))))

		     (local-init (%time %state-offset)
		       (setf %current-step 0)
		       ,local-init)

		     (dispatch (%time %sys-call)
		       (case= %current-step
			 ,@(loop :for step :in steps
			      :for o :in step-offsets
			      :for i :from 0 :collect
			      `(,i (,step %time ,o %sys-call))))))
	    (case= %sys-call
	      (+step+ (dispatch %time %sys-call))
	      (+collect-state-info+
	       (+ (how-many-slots) (reduce #'+ (slot-counts-from-steps))))
	      (+init+ (init %state-offset))
	      (+local-init+ (local-init %time %state-offset)
			    (dispatch %time %sys-call))
	      (otherwise (error "invalid %sys-call bug"))))))))

(def-temporal-clause before (rel-time)
  :state-vars (start deadline)

  :local-init
  (lambda (time-already-passed)
   (setf start (- now time-already-passed)
         deadline (+ now (- rel-time time-already-passed))))

  :progress
  (/ (- now start) (- deadline start))

  :body
  (progn
    (incf time to-eat)
    (if (< time deadline)
        (print "Hi")
        (next (- deadline time)))))

(defmacro def-temporal-clause (name args &body keys)
  (destructuring-bind (&key state-vars local-init progress body) keys
    `(defmacro ,name (,@args &body body)
       (let ((local-init ',local-init)
             (progress ',progress)
             (body ',body))
         `(macrolet (,@(loop :for v :in ',state-vars
                          :for i :from ,(length state-vars) :collect
                          `(,v (aref state (+ %state-offset ,i)))))
            (labels ((local-init (%time %state-offset)
                       ,local-init)

                     (dispatch (%time %sys-call)
                       (let ((%progress% ,progress))
                         ,@body)))
              (case= %sys-call
                (+step+ (dispatch %time %sys-call))
                (+local-init+ (local-init %time %state-offset)
                              (dispatch %time %sys-call))
                (+collect-state-info+ ,,(length state-vars))
                (+init+ nil)
                (otherwise (error "invalid %sys-call bug")))))))))



(defun rolling-add (list)
  (reverse (maplist λ(apply #'+ _) (reverse list))))

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
