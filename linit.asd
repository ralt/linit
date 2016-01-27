(asdf:defsystem #:linit
  :description "An init system written in Common Lisp"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :serial t
  :depends-on (:sb-posix :cffi :swank)
  :build-operation asdf:program-op
  :build-pathname "sbin/init"
  :entry-point "linit:main"
  :components ((:file "package")
               (:file "service")
               (:file "graph")
               (:file "linit")))
