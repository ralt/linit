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

(defvar *sigchld* 17)

(defun main (args)
  (declare (ignore args))
  (load-services #p"/lib/linit/*.lisp")
  (with-signal-handler *sigchld*
      (lambda ()
        (loop
           (handler-case
               (multiple-value-bind (pid status)
                   (sb-posix:waitpid -1 sb-posix:wnohang)
                 (when (= pid 0)
                   (return))
                 (setf
                  (state (find-service-by-pid pid))
                  (cond
                    ((sb-posix:wifexited status)
                     (if (= (sb-posix:wexitstatus status) 0)
                         'stopped 'errored))
                    (t 'errored))))
             (sb-posix:syscall-error () (return)))))
    (mapcar #'start-service *services*)
    (sb-impl::toplevel-repl nil)))
