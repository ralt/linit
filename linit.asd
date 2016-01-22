(asdf:defsystem #:linit
  :description "An init system written in Common Lisp"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:sb-posix :sb-thread)
  :components ((:file "package")
               (:file "linit")))
