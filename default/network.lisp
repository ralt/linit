(defservice network
    :before-stopped '(remount-fs)
    :start (lambda ()
             (ensure-directories-exist #p "/run/network/")
             (uiop:run-program
              (format nil
                      "/sbin/ifup --interfaces /etc/network/interfaces ~{~A ~}"
                      (interfaces)))))

(defun interfaces (&optional (pointer nil))
  (if pointer
      (cffi:with-foreign-slots ((ifa-name ifa-next)
                                (cffi:mem-ref pointer :pointer)
                                (:struct ifaddrs))
        (append (list ifa-name)
                (unless (cffi:null-pointer-p ifa-next)
                  (interfaces ifa-next))))
      (cffi:with-foreign-object (ifaddrs '(:pointer (:struct ifaddrs)))
        (cffi:with-foreign-object (ifaddrs-pointer :pointer)
          (setf (cffi:mem-ref ifaddrs-pointer :pointer) ifaddrs)
          (cffi:foreign-funcall "getifaddrs" :pointer ifaddrs-pointer :int)
          (cffi:with-foreign-slots ((ifa-name ifa-next)
                                    (cffi:mem-ref ifaddrs-pointer :pointer)
                                    (:struct ifaddrs))
            (remove-duplicates
             (append (list ifa-name)
                     (unless (cffi:null-pointer-p ifa-next)
                       (interfaces ifa-next)))
             :test #'string=))))))
