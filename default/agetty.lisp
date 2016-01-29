(defservice agetty
    :start (lambda ()
             (exec "/sbin/agetty tty1")))
