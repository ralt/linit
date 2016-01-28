(defservice hostname
    :start (lambda ()
             (with-open-file (hostname #p "/etc/hostname" :direction :input)
               (with-open-file (kernel-hostname #p "/proc/sys/kernel/hostname"
                                                :direction :output)
                 (write-line (read-line hostname) kernel-hostname)))))
