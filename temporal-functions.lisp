;;;; temporal-functions.lisp

(in-package #:temporal-functions)


;; {TODO} Add paramter for time sources
;; {TODO} add once, between, each, once, whilst
;; {TODO} if user fires expired inside body of clause the effect should be 
;;        local

(defclass result ()
  ((closed-vars :initarg :closed-vars :accessor closed-vars)
   (start-test :initarg :start-test :accessor start-test)
   (expire-test :initarg :expire-test :accessor expire-test)
   (funcs :initarg :funcs :accessor funcs)
   (init :initarg :init :accessor init)
   (body :initarg :body :accessor body)))

(defmacro with-compile-result (form &body wcr-body)
  `(with-slots (closed-vars
                (start-tests start-test)
                (expire-tests expire-test)
                funcs
                (init-funcs init) body)
       ,form
     (declare (ignorable closed-vars start-tests expire-tests funcs
                         init-funcs body))
     ,@wcr-body))

(defun new-result (&key closed-vars start-test expire-test funcs init body)
  (make-instance
   'result
   :closed-vars closed-vars
   :start-test (list start-test)
   :expire-test (list expire-test)
   :funcs funcs
   :init (list init)
   :body body))

(defun empty-result ()
  (make-instance 'result :closed-vars nil :start-test nil
                 :expire-test nil :funcs nil :init nil :body nil))

(defun clean-result (result)
  (setf (closed-vars result) (remove nil (closed-vars result))
        (start-test result) (remove nil (start-test result))
        (expire-test result) (remove nil (expire-test result))
        (funcs result) (remove nil (funcs result))
        (init result) (remove nil (init result)))
  result)

(defun merge-results (results &optional first-overrides-body-form)
  (let* ((result (empty-result)))
    (setf (closed-vars result)
          (remove-duplicates (mapcan #'closed-vars results)))
    (setf (start-test result)
          (remove-duplicates (mapcan #'start-test results)))
    (setf (expire-test result)
          (remove-duplicates (mapcan #'expire-test results)))
    (setf (funcs result)
          (remove-duplicates (mapcan #'funcs results)))
    (setf (init result)
          (remove-duplicates (mapcan #'init results)))
    (if first-overrides-body-form
        (setf (body result) (body (first results)))
        (setf (body result) `(progn ,@(mapcar #'body results))))
    (clean-result result)))

(defparameter *temporal-clause-expanders*
  (make-hash-table))
(defparameter *default-time-source* 'get-internal-real-time)
(defparameter *time-var* '|time|)
(defparameter *init-arg* '|start-time|)
(defparameter *progress-var* '|progress|)

(defmacro def-t-expander (name args &body body)
  (let ((ename (symb name '-expander)))
    `(progn
       (defun ,ename ,args
         ,@body)
       (setf (gethash ,(kwd name) *temporal-clause-expanders*) #',ename))))

(defun gen-then-step (compile-result step-num start-var top step-var)
  (with-compile-result compile-result
    (let ((init-name (caar init-funcs))
          (expired-name (caar expire-tests)))
      `((,step-num
         (,init-name (,start-var))
         (incf ,step-var)
         (go ,top))
        (,(1+ step-num)
          (if (,expired-name)
              (progn (incf ,step-var) (go ,top))
              (progn ,body)))))))

(def-t-expander then (&rest forms)
  (let ((step-var (gensym "step"))
        (start-var (gensym "start"))
        (expire-test-name (gensym "expired"))
        (init-name (gensym "init"))
        (advance-step (gensym "advance-step"))
        (compiled-forms (mapcar #'process-t-body forms))
        (top (gensym "top")))
    (merge-results
     (cons (new-result
            :closed-vars `((,step-var 0)
                           (,start-var 0))
            :expire-test `(,expire-test-name () (> ,step-var
                                                   ,(1- (* 2 (length forms)))))
            :init `(,init-name (,*init-arg*) (setf ,start-var ,*init-arg*))
            :funcs `((,start-var () ,start-var)
                     (,advance-step
                      ()
                      (tagbody
                         ,top
                         (case ,step-var
                           ,@(loop :for i :from 0 :by 2
                                :for s :in (cons start-var
                                                 (mapcar (λ caar (expire-test %))
                                                         compiled-forms))
                                :for c :in compiled-forms :append
                                (gen-then-step c i s top step-var))))))
            :body `(,advance-step))
           compiled-forms)
     t)))

(defun gen-repeat-step (compile-result step-num start-var top step-var)
  (with-compile-result compile-result
    (let ((init-name (caar init-funcs))
          (expired-name (caar expire-tests)))
      `((,step-num
         (,init-name (,start-var))
         (incf ,step-var)
         (go ,top))
        (,(1+ step-num)
          (if (,expired-name)
              (progn (incf ,step-var) (go ,top))
              (progn ,body)))))))

(def-t-expander repeat (&rest forms)
  (let ((step-var (gensym "step"))
        (start-var (gensym "start"))
        (expire-test-name (gensym "expired"))
        (init-name (gensym "init"))
        (advance-step (gensym "advance-step"))
        (compiled-forms (mapcar #'process-t-body forms))
        (top (gensym "top")))
    (merge-results
     (cons (new-result
            :closed-vars `((,step-var 0)
                           (,start-var 0))
            :expire-test `(,expire-test-name () (> ,step-var
                                                   ,(1- (* 2 (length forms)))))
            :init `(,init-name (,*init-arg*) (setf ,start-var ,*init-arg*))
            :funcs `((,start-var () ,start-var)
                     (,advance-step
                      ()
                      (tagbody
                         ,top
                         (case ,step-var
                           ,@(loop :for i :from 0 :by 2
                                :for s :in (cons start-var
                                                 (mapcar (λ caar (expire-test %))
                                                         compiled-forms))
                                :for c :in compiled-forms :append
                                (gen-repeat-step c i s top step-var))
                           `(,(length compiled-forms)
                             (setf ,step-var 0)
                              (go ,top))))))
            :body `(,advance-step))
           compiled-forms)
     t)))

(def-t-expander before (deadline &rest body)
  (let* ((start-var (gensym "before-start"))
         (deadline-var (gensym "before-deadline"))
         (start-test-name (gensym "start"))
         (expire-test-name (gensym "expired"))
         (init-name (gensym "init-before"))
         (compiled-body (mapcar #'process-t-body body)))
    (merge-results
     (cons (new-result
            :closed-vars `((,deadline-var 0)
                           (,start-var 0))
            :start-test `(,start-test-name () t)
            :expire-test `(,expire-test-name () (when (>= ,*time-var* ,deadline-var)
                                                  ,deadline-var))
            :init `(,init-name (,*init-arg*) ;; last argument is always time overflow
                               (setf ,start-var ,*init-arg*
                                     ,deadline-var (+ ,*init-arg* ,deadline))
                               ,@(loop :for c :in compiled-body
                                    :if (caar (init c))
                                    :collect `(,(caar (init c)) ,*init-arg*)))
            :body `(when (not (,expire-test-name))
                     (let ((,*progress-var*
                            (float (- 1.0 (/ (- ,deadline-var ,*time-var*)
                                             (- ,deadline-var ,start-var))))))
                       (declare (ignorable ,*progress-var*))
                       (progn ,@(mapcar #'body compiled-body)))))
           compiled-body)
     t)))

(def-t-expander after (delay &rest body)
  (let* ((after-var (gensym "after-delay"))
         (start-test-name (gensym "start"))
         (expire-test-name (gensym "expired"))
         (init-name (gensym "init-after"))
         (first-run (gensym "first-run-after"))
         (compiled-body (mapcar #'process-t-body body)))
    (merge-results
     (cons (new-result
            :closed-vars `((,after-var 0)
                           (,first-run t))
            :start-test `(,start-test-name () (when (>= ,*time-var* ,after-var)
                                                ,after-var))
            :expire-test `(,expire-test-name () nil)
            :init `(,init-name (,*init-arg*)
                               (setf ,after-var (+ ,*init-arg* ,delay)))
            :body `(when (and (not (,expire-test-name)) (,start-test-name))
                     (when ,first-run
                       (setf ,first-run nil)
                       ,@(loop :for c :in compiled-body
                            :if (caar (init c))
                            :collect `(,(caar (init c)) ,after-var)))
                     (let ((,*progress-var* 1))
                       (declare (ignorable ,*progress-var*))
                       (progn ,@(mapcar #'body compiled-body)))))
           compiled-body)
     t)))

;;this was in the old version of each
;; (make-stepper ,timestep
;;   ,@(when max-cache-size
;;       (list max-cache-size)))

;; {TODO} we allow swapping out of time source at runtime. Examine the
;;        usefulness of this we may be able to see a higher good in this.

(defun make-stepper (step-size &optional (max-cache-size (max (* 10 step-size) 10000.0))
                                 (default-source *default-time-source*))
  "this takes absolute sources"
  ;; if max-cache-size is set to zero
  (when (< max-cache-size step-size)
    (error "Make-Stepper: max-cache-size is smaller than step-size.~%max-cache-size: ~a~%step-size: ~a~%" max-cache-size step-size))
  (let ((time-cache 0)
        (last-val (funcall default-source)))
    (lambda (&optional (time-source default-source))
      (let* ((time (abs (funcall time-source)))
             (dif (- time last-val)))
        (setf last-val time
              time-cache (min max-cache-size (+ time-cache dif)))
        (when (>= time-cache step-size)
          (setf time-cache (- time-cache step-size))
          (min 1.0 (/ time-cache step-size)))))))

(def-t-expander each (delay &rest body)
  (let* ((start-test-name (gensym "start"))
         (expire-test-name (gensym "expired"))
         (init-name (gensym "init-each"))
         (stepper-var (gensym "stepper")))
    (new-result
     :closed-vars `((,stepper-var nil))
     :start-test `(,start-test-name () t)
     :expire-test `(,expire-test-name () nil)
     :init `(,init-name (,*init-arg*) ;; {TODO} starttime of stepper should be *init-arg*
                        (declare (ignore ,*init-arg*))
                        (setf ,stepper-var (make-stepper ,delay)))
     :body `(when (funcall ,stepper-var)
              (let ((,*progress-var* 1))
                (declare (ignorable ,*progress-var*))
                (progn ,@body))))))


;;----------------------------------------------------------------------------

(defun process-t-body (form)
  (cond ((atom form) (new-result :body form))
        ((eql (first form) 'quote) (new-result :body form))
        ((gethash (first form) *temporal-clause-expanders*)
         (apply (gethash (first form) *temporal-clause-expanders*) (rest form)))
        (t (new-result :body form))))

(defun improve-readability (form)
  (cond ((atom form) form)
        ((and (eql 'progn (first form)) (= (length form) 2))
         (improve-readability (second form)))
        (t (cons (improve-readability (first form))
                 (improve-readability (rest form))))))


(defun tbody (compiled)
  `(let ((,*time-var* (,*default-time-source*)))
     (declare (ignorable ,*time-var*))
     (labels (,@(mapcan #'start-test compiled)
              ,@(mapcan #'expire-test compiled)
                ,@(mapcan #'funcs compiled))
       (prog1
           ,(improve-readability `(progn ,@(mapcar #'body compiled)))
         (when (and ,@(loop :for c :in compiled
                         :collect `(,(caar (start-test c)))
                         :collect `(,(caar (expire-test c)))))
           (signal-expired))))))

(defun tcompile (body)
  (mapcar #'process-t-body
          (macroexpand-dammit:macroexpand-dammit
           `(macrolet ((before (&body b) `(:before ,@b))
                       (after (&body b) `(:after ,@b))
                       (then (&body b) `(:then ,@b))
                       (each (&body b) `(:each ,@b)))
              ,body))))

(defun t-init-base (compiled)
  (mapcar (lambda (x) `(,x (,*default-time-source*)))
          (mapcar #'caar (mapcar #'init compiled)))) ;cddar

(defmacro tlambda (args &body body)
  (let ((compiled (tcompile body)))
    `(let* (,@(mapcan #'closed-vars compiled))
       (labels ,(reverse (mapcan #'init compiled))
         (let ((func (lambda ,args ,(tbody compiled))))
           ,@(t-init-base compiled)
           func)))))

(defmacro tdefun (name args &body body)
  (unless name (error "temporal function must have name"))
  (let ((compiled (tcompile body)))
    `(let ,(mapcan #'closed-vars compiled)
       (labels ,(reverse (mapcan #'init compiled))
         (defun ,name ,args ,(tbody compiled))
         ,@(t-init-base compiled)
         ',name))))

(defmacro before (deadline &body body)
  `(tlambda () (before ,deadline ,@body)))

(defmacro after (delay &body body)
  `(tlambda () (after ,delay ,@body)))

(defmacro each (delay &body body)
  `(tlambda () (each ,delay ,@body)))

;;--------------------------------------------------------------------

(define-condition c-expired (condition) ())

(defun signal-expired () (signal 'c-expired) nil)

(defmacro expiredp (&body body)
  `(handler-case (progn ,@body nil)
     (c-expired (c) (progn c t))))

(defmacro expiredp+ (&body body)
  `(handler-case (values nil (progn ,@body))
     (c-expired (c) (progn c t))))

;;--------------------------------------------------------------------

