(defservice remount-fs
  :start (lambda () (run-program "mount" '("-a"))))
