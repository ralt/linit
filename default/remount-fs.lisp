(defclass fstab-entry ()
  ((device :type string :initarg :device :reader device)
   (mount-point :type string :initarg :mount-point :reader mount-point)
   (filesystem :type string :initarg :filesystem)
   (options :type string :initarg :options :reader options)
   (dump :type string :initarg :dump)
   (pass :type string :initarg :pass)))

(defun fstab-entries ()
  (with-open-file (f #p "/etc/fstab" :direction :input)
    (loop for line = (read-line f nil 'eof)
       until (eq line 'eof)
       ;; This should be enough to assume we're on a correct line
       when (and
             (> (length line) 0)
             (not (eq (elt line 0) #\#))
             (= (length (ppcre:split "\\s+" line)) 6))
       collect (destructuring-bind
                     (device mount-point filesystem options dump pass)
                   (ppcre:split "\\s+" line)
                 (make-instance 'fstab-entry
                                :device device
                                :mount-point mount-point
                                :filesystem filesystem
                                :options options
                                :dump dump
                                :pass pass)))))

(defservice remount-fs
    :start (lambda ()
             (cffi:define-foreign-library libmount
               (t (:default "libmount")))
             (cffi:use-foreign-library libmount)
             (dolist (entry (fstab-entries))
               (cffi:with-foreign-object (ctx :pointer)
                 (setf ctx (cffi:foreign-funcall "mnt_new_context" :pointer))
                 (cffi:foreign-funcall "mnt_context_set_options"
                                       :pointer ctx
                                       :string (format nil "remount,~A"
                                                       (options entry)))
                 (cffi:foreign-funcall "mnt_context_set_target"
                                       :pointer ctx
                                       :string (mount-point entry)
                                       :void)
                 (when
                     (cffi:foreign-funcall "mnt_context_mount"
                                           :pointer ctx
                                           :int)
                   (syslog:log "linit-remount-fs" :user :err
                               (format
                                nil
                                "Failed to mount ~A on ~A"
                                (device entry) (mount-point entry))))
                 (cffi:foreign-funcall "mnt_free_context"
                                       :pointer ctx
                                       :void)))))
