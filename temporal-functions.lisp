;;;; temporal-functions.lisp

(in-package #:temporal-functions)

(defun new-result (&key closed-vars start-test expire-test init body)
  `(,closed-vars (,start-test) (,expire-test) (,init) (,body)))


(defun merge-results (results &optional first-overrides-body-form)
  (let ((merged (reduce #'(λ mapcar #'cons %1 %)
                        results
                        :initial-value (make-list (length (first results))))))
    (if first-overrides-body-form
        (append (butlast merged) (list (last (first results)))))))

(defun compile-and-merge (forms &optional (called-by-temporal-clause t))
  (let ((results (mapcar (λ process-t-body % called-by-temporal-clause) forms)))
    (merge-results results)))


(defparameter *temporal-clause-expanders*
  (make-hash-table))
(defparameter *time-var* '|time|)
(defparameter *progress-var* '|progress|)

(defmacro def-t-expander (name args &body body)
  (let ((ename (symb name '-expander)))
    `(progn
       (defun ,ename ,args
         ,@body)
       (setf (gethash ',name *temporal-clause-expanders*) #',ename))))

(def-t-expander then (&rest forms)
  (let ((step-var (gensym "step"))
        (start-var (gensym "start"))
        (expire-test-name (gensym "expired"))
        (init-name (gensym "init"))
        (advance-step (gensym "advance-step"))
        (compiled-forms (mapcar #'(λ process-t-body % t) forms))
        (top (gensym "top")))
    (new-result
     :closed-vars `((,step-var 0))
     :expire-test `(,expire-test-name () (> ,step-var ,(1- (* 2 (length forms)))))
     :init `(,init-name (start-time) (setf ,start-var start-time))
     :body `(labels ((,advance-step ()
                       (tagbody 
                          ,top                          
                          (case step
                            ,(loop :for )
                            (0 (init-0 ,start-var) 
                               (incf step) 
                               (go ,top))
                            (1 (if (expired-0)
                                   (progn (incf step) (go ,top))
                                   (do-step-0)))
                            (2 (init-1 (expired-0)) (incf step) (go ,top))
                            (3 (if (expired-1)
                                   (progn (incf step) (go ,top))
                                   (do-step-1)))
                            (4 (init-2 (expired-1)) (incf step) (go ,top))
                            (5 (if (expired-2)
                                   (progn (incf step) (go ,top))
                                   (do-step-2)))))))
              (,advance-step)))))



(def-t-expander before (deadline &rest body)
  (let* ((start-var (gensym "before-start"))
         (deadline-var (gensym "before-deadline"))
         (start-test-name (gensym "start"))
         (expire-test-name (gensym "expired"))
         (init-name (gensym "init-before"))
         (compiled-body (mapcar #'(λ process-t-body % t) body)))
    (merge-results
     (cons (new-result 
            :closed-vars `((,deadline-var ,deadline)
                           (,start-var nil))
            :start-test `(,start-test-name () t)
            :expire-test `(,expire-test-name () (when (>= ,*time-var* ,deadline-var)
                                                  ,deadline-var))
            :init `(,init-name (deadline start-time) ;; last argument is always time overflow
                               (setf ,start-var start-time
                                     ,deadline-var deadline))
            :body `(when (not (,expire-test-name))
                     (let ((,*progress-var* 
                            (float (- 1.0 (/ (- ,deadline-var ,*time-var*)
                                             (- ,deadline-var ,start-var)))))))
                     ,@(last compiled-body)))
           compiled-body)
     t)))

(defun process-t-body (form &optional called-by-temporal-clause)
  (declare (ignore called-by-temporal-clause))
  (cond ((atom form) form)
        ((eql (first form) 'quote) form)
        (t (let ((tmp (gethash (first form) *temporal-clause-expanders*)))
             (if tmp
                 (apply tmp (rest form))
                 (mapcar #'process-t-body form))))))


(defmacro with-compile-result (form &body body)
  `(destructuring-bind (closed-vars start-tests expire-tests init-funcs body)
       ,form
     (declare (ignorable closed-vars start-tests expire-tests init-funcs body))
     ,@body))

(defmacro tfun (name args &body body)
  (let* ((expand-macros nil)
         (expanded-body (macroexpand-dammit:macroexpand-dammit 
                         `(macrolet ,expand-macros ,@body))))
    (with-compile-result (process-t-body expanded-body)
      `(let ,closed-vars
         (,@(if name `(defun ,name) '(lambda)) ,args
            (labels (,@start-tests
                     ,@expire-tests
                     ,@init-funcs)
              ,@body))))))

;; outside of tfun there are functions (or macros) for before, after between 
;; etc which create tlambdas.
;; So tfun doesnt infinitely recurse, each temporal clause must has a defmacro
;; that expands to an alternate but equivilent symbol


