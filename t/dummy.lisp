(defservice dummy
  :before '(remount-fs)
  :start (lambda ()
           (with-open-file (f #p"dummy"
                              :direction :output
                              :if-exists :overwrite)
             (write-sequence "dummy" f))))
