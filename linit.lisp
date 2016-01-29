(in-package #:linit)

;;; Inspired and sligthly adapted from
;;; https://github.com/orthecreedence/cl-signal-handler
(defmacro set-signal-handler (signo &body body)
  "Replace the current handler for the signal number under signo, and return a
  pointer to the handler that is being replaced."
  (let ((handler (gensym "HANDLER")))
    `(progn
       (cffi:defcallback ,handler :void ((signo :int))
         (declare (ignore signo))
         ,@body)
       (cffi:foreign-funcall
        "signal" :int ,signo :pointer (cffi:callback ,handler) :pointer))))

(defmacro with-signal-handler (signo handler-fn &body body)
  "Allows you to execute a block of code using the signal handling function
  specified by handler-fn. Once the body is finished, the original signal
  handler will always be restored."
  (let ((default-handler (gensym)))
    `(let ((,default-handler (set-signal-handler ,signo (funcall ,handler-fn))))
       (unwind-protect
            (progn ,@body)
         (cffi:foreign-funcall
          "signal" :int ,signo :pointer ,default-handler :pointer)))))

(defun main ()
  (with-signal-handler sb-posix:sigchld
      (lambda ()
        (loop
           (handler-case
               (multiple-value-bind (pid status)
                   (sb-posix:waitpid -1 sb-posix:wnohang)
                 (when (= pid 0)
                   (return))
                 (let ((service (find-service-by-pid pid)))
                   (setf
                    (state service)
                    (if (and (sb-posix:wifexited status)
                             (= (sb-posix:wexitstatus status) 0))
                        'stopped
                        'errored))
                   (format t "~A changed to state ~A.~%"
                           (name service) (state service))))
             (sb-posix:syscall-error () (return)))))
    (load-services #p"/lib/linit/*.lisp")
    (start-services)
    (swank:create-server :port 4 :style nil :dont-close t)))
