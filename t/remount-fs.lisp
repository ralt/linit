(defservice remount-fs
  :start (lambda () (sb-ext:run-program "mount" '("-a"))))
