(cffi:defcstruct (ifaddrs :size 56)
  (ifa-next :pointer :offset 0)
  (ifa-name :string :offset 8))

(defun interfaces ()
  (let ((names nil))
    (cffi:with-foreign-object (ifaddrs '(:pointer (:struct ifaddrs)))
      (cffi:with-foreign-object (ifaddrs-pointer :pointer)
        (setf (cffi:mem-ref ifaddrs-pointer :pointer) ifaddrs)
        (cffi:foreign-funcall "getifaddrs" :pointer ifaddrs-pointer :int)
        (cffi:with-foreign-slots ((ifa-name ifa-next)
                                  (cffi:mem-ref ifaddrs-pointer :pointer)
                                  (:struct ifaddrs))
          (push ifa-name names)
          (setf ifaddrs-pointer ifa-next)
          (loop
             do (cffi:with-foreign-slots ((ifa-name ifa-next)
                                          (cffi:mem-ref ifaddrs-pointer :pointer)
                                          (:struct ifaddrs))
                  (push ifa-name names)
                  (if (cffi:null-pointer-p ifa-next)
                      (return-from interfaces (remove-duplicates names
                                                                 :test #'string=))
                      (setf ifaddrs-pointer ifa-next)))))))))

(defservice network
    :before-stopped '(remount-fs)
    :start (lambda ()
             (ensure-directories-exist #p "/run/network/")
             (uiop:run-program
              (format nil
                      "/sbin/ifup --interfaces /etc/network/interfaces ~{~A ~}"
                      (interfaces)))))
