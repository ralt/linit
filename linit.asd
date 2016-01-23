(asdf:defsystem #:linit
  :description "An init system written in Common Lisp"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:sb-posix)
  :components ((:file "package")
               (:file "linit")))
