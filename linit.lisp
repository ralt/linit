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
                   (if service
                       (progn
                         (setf
                          (state service)
                          (if (and (sb-posix:wifexited status)
                                   (= (sb-posix:wexitstatus status) 0))
                        'stopped
                        'errored))
                         (format t "~A changed to state ~A.~%"
                                 (name service) (state service))
                         ;; Explicitly do this *after* setting the new state
                         (when (eq (state service) 'stopped)
                           (let ((dag (make-dag *services*)))
                             (loop for child across
                                  (children
                                   (find-graph-element-by-service-name
                                    dag (name service)))
                                do (start-graph-services child)))))
                       ;; This means the parent's code after (fork)
                       ;; didn't run yet.
                       ;; We're pushing this pid in a global hash table
                       ;; that will be checked by the parent.
                       (setf
                        (gethash pid *sigchld-pids*)
                        (if (and (sb-posix:wifexited status)
                                 (= (sb-posix:wexitstatus status) 0))
                            'stopped
                            'errored)))))
             (sb-posix:syscall-error () (return)))))
    (load-services #p"/lib/linit/*.lisp")
    (start-services *services*)
    (sb-impl::toplevel-repl nil)
    (swank:create-server :port 4 :style nil :dont-close t)))
