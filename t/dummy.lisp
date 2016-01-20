(defservice dummy
  :depends-on '(remount-fs)
  :start (lambda ()
           (with-open-file (f #p"/dummy" :direction :output)
             (write-sequence "dummy" f))))
