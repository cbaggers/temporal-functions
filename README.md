temporal-functions
==================

Rewrite of the temporal function code from cepl.

Adds tlambda & tdefun along with a small collection of related funcs and macros


tlambda is a lambda with an internal concept of time

    (tlambda () (before (seconds 10) (print "hi")))

Gives a function that will work for the next 10 seconds. After that with will return `nil` and release an 'expired' signal.

    (expiredp *) ;; will return true if function has expired

Can be fairly complicated and will expand into a fast state-machine, by only using the signal mechanism if all stages have expired.

    (tlambda ()
      (then
       (before (seconds 3)
         (print "hi"))
       (before (seconds 3)
         (print "still here"))
       (before (seconds 3)
         (print "Bored now"))))

Also has brevity macros

    (before (millseconds 1300)
      (print "ping!"))

    ;; expands to

    (tlambda ()
      (before (millseconds 1300)
        (print "ping!")))

    ;; which, in turn, expands to a form equivilent to

    (let* ((deadline 0) (start 0))
      (labels ((init-before (start-time)
                 (setf start start-time
                       deadline (+ start-time (millseconds 1300)))))
        (let ((func
               (lambda ()
                 (let ((time (get-internal-real-time)))
                   (labels ((expired () (when (>= time deadline)
                                          deadline)))
                     (if (expired)
                         (signal-expired)
                         (print "ping!")))))))
          (init-before (get-internal-real-time))
          func)))


Background
----------

I had spent an afternoon reading about and sketching out different ways obstracting time. My mind must have been working on something without my input as I suddenly heard the words "Temporal Lambda" in my head.

The 'voice' stopped me in my tracks and I started wondering what temporal lambdas were...or what they could be. The idea of temporal scope sounded weird too. I decided the only way to resolve this way to build the constructs and see if they turn out to be useful.

Use will prove this useful or delusional :)
