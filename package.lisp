(defpackage #:linit
  (:use #:cl)
  (:export :main
           :defservice))

(defpackage #:linit-user
  (:use #:cl #:linit))
