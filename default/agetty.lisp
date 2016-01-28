(defservice agetty
    :start (lambda ()
             (sb-ext:run-program "exec" '("/sbin/agetty" "tty1"))))
