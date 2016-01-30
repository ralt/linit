(defservice remount-fs
    :start (lambda ()
             (dolist (entry (fstab-entries))
               (uiop:run-program
                (format nil "/bin/mount -o remount,~A ~A ~A"
                        (options entry) (device entry) (mount-point entry))))))

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
                     (device mount-point type options dump pass)
                   (ppcre:split "\\s+" line)
                 (make-instance 'fstab-entry
                                :device device
                                :mount-point mount-point
                                :type type
                                :options options
                                :dump dump
                                :pass pass)))))

(defclass fstab-entry ()
  ((device :type string :initarg :device :reader device)
   (mount-point :type string :initarg :mount-point :reader mount-point)
   (type :type string :initarg :type :reader entry-type)
   (options :type string :initarg :options :reader options)
   (dump :type string :initarg :dump :reader dump)
   (pass :type string :initarg :pass :reader pass)))
