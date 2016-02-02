(defpackage #:linit
  (:use #:cl)
  (:export :main

           ;; service
           :defservice
           :list-services
           :pid
           :state
           :name
           :start
           :before
           :after
           :before-stopped
           :after-stopped
           :find-service
           :find-service-by-pid
           :replace-service
           :add-service
           :load-services
           :load-service
           :start-services
           :start-service

           ;; graph
           :make-dag
           :root-elements
           :has-cycle
           :start-graph-services
           :service
           :children
           :parents
           :find-graph-element-by-service-name))

(defpackage #:linit-user
  (:use #:cl #:linit)
  (:export :reboot
           :halt
           :exec))
