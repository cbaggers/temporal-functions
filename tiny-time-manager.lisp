(in-package :tiny-time-manager)

;; This is meant as an example rather than for serious use.

(defstruct (temporal-func-pool (:constructor %make-tfp))
  (funcs (list t) :type list))

(defun make-tfunc-pool ()
  (%make-tfp))

;;{TODO} The funcall should catch errors and offer to remove the function
;; from the time manager
(let ((entries (make-tfunc-pool)))

  (defun expose (&optional pool)
    (let ((pool (or pool entries)))
      (rest (temporal-func-pool-funcs pool))))

  (defun update (&optional pool)
    (let* ((pool (or pool entries))
	   (entries (temporal-func-pool-funcs pool))
	   (last entries)
	   (current (cdr entries)))
      (loop :until (null current) :do
         (if (restart-case (expiredp (funcall (car current)))
               (remove-managed-tfunction () t)) ;; {TODO} what is this?
             (setf (cdr last) (cdr current)
                   last current)
             (setf (cdr last) current
                   last current))
         (setf current (cdr current)))
      t))

  (defun add (item &optional pool)
    (let* ((pool (or pool entries)))
      (setf (temporal-func-pool-funcs pool)
	    (append (temporal-func-pool-funcs pool)
		    (list item)))
      item))

  (defun release (item &optional pool)
    (let* ((pool (or pool entries)))
      (setf (temporal-func-pool-funcs pool)
	    (delete item (temporal-func-pool-funcs pool)))))

  (defun clean (&optional pool)
    (let* ((pool (or pool entries)))
      (setf (temporal-func-pool-funcs pool)
	    (list t)))))
