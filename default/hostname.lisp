(defservice hostname
    :start (lambda ()
             (with-open-file (f #p "/etc/hostname" :direction :input)
               (let ((hostname (read-line f)))
                 (cffi:foreign-funcall "sethostname"
                                       :string hostname
                                       :int (length hostname)
                                       :int)))))
