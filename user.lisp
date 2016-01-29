(in-package #:linit-user)

(defun reboot ()
  (sb-posix:sync)
  (cffi:foreign-funcall "reboot"
                        :int #x1234567
                        :int))

(defun halt ()
  (sb-posix:sync)
  (cffi:foreign-funcall "reboot"
                        :int #x4321fedc
                        :int))

(defun exec (command)
  (let ((parts (uiop:split-string command)))
    (cffi:with-foreign-object (argv :string (length parts))
      (dotimes (i (length parts))
        (setf (cffi:mem-aref argv :string i) (nth i parts)))
      (cffi:foreign-funcall "execvp"
                            :string (first parts)
                            :pointer argv
                            :int)))
  (error "exec failed"))
