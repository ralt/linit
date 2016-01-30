(defservice remount-fs
    :start (lambda ()
             (with-open-file (f #p "/etc/fstab" :direction :input)
               (loop for line = (read-line f nil 'eof)
                  until (eq line 'eof)
                  ;; This should be enough to assume we're on a correct line
                  when (and
                        (> (length line) 0)
                        (not (eq (elt line 0) #\#))
                        (= (length (ppcre:split "\\s+" line)) 6))
                  do (destructuring-bind
                           (device mount-point type options dump pass)
                         (ppcre:split "\\s+" line)
                       (declare (ignore type dump pass))
                       (unless (string= mount-point "none")
                         (sb-ext:run-program
                          "/bin/mount" (list
                                        "-o" (format nil "remount,~A" options)
                                        device
                                        mount-point))))))))
