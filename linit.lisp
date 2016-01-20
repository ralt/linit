(in-package #:linit)

(defvar *stopped-services* nil)
(defvar *started-services* nil)

(defmacro defservice (name &rest initargs)
  `(push (make-instance 'service
                        :name ',name
                        ,@initargs)
         *stopped-services*))

(defclass service ()
  ())

(defun load-services (path)
  (dolist (service (directory path))
    ;; For some reason, this needs to be re-applied for the load'ed
    ;; file to be in the correct package.
    (in-package #:linit)
    (load service)))

(defun start-service (service)
  )

(defun main (args)
  (declare (ignore args))
  (load-services #p"/lib/linit/*.lisp")
  (dolist (service *stopped-services*)
    (start-service service))
  (sb-impl::toplevel-repl nil))
