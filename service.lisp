(in-package #:linit)

(defvar *services* nil)
(defvar *sigchld-pids* (make-hash-table))

(deftype service-state ()
  '(member started stopped errored))

(defclass service ()
  ((pid :accessor pid :type integer :initform nil)
   (state :accessor state :type service-state :initform nil)
   (name :reader name :initarg :name :type symbol)
   (start :initarg :start :reader start :type function :initform nil)
   (before :initarg :before :reader before :type list :initform nil)
   (after :initarg :after :reader after :type list :initform nil)
   (before-stopped :initarg :before-stopped :reader before-stopped :type list
                   :initform nil)
   (after-stopped :initarg :after-stopped :reader after-stopped :type list
                  :initform nil)))

(defmethod initialize-instance :after ((service service) &key)
  "Puts the (before|after)-stopped slots in (before|after)
   so that the graph is properly built."
  (when (before-stopped service)
    (setf (slot-value service 'before) (before-stopped service)))
  (when (after-stopped service)
    (setf (slot-value service 'after) (after-stopped service))))

(defmacro defservice (name &rest initargs)
  (let ((new-service (gensym)))
    `(let ((,new-service (make-instance 'service
                                       :name ',name
                                       ,@initargs)))
       (export ',name)
       (if (find-service ',name)
           (replace-service ,new-service)
           (add-service ,new-service)))))

(defun list-services ()
  *services*)

(defun find-service (name)
  (find-if (lambda (service)
             (eq (name service) name))
           *services*))

(defun find-service-by-pid (pid)
  (find-if (lambda (service)
             (eq (pid service) pid))
           *services*))

(defun replace-service (new-service)
  (setf *services* (list
                    (remove-if (lambda (service)
                                 (eq (name service) (name new-service)))
                               *services*)
                    new-service)))

(defun add-service (service)
  (push service *services*))

(defun load-services (path)
  (mapc #'load-service (directory path)))

(defun load-service (service)
  (let ((*package* (find-package "LINIT-USER")))
    (load service)))

(defun start-services (services)
  "Starts the services while respecting the dependencies.

This is kinda the big job of linit, so let's take some time
and explain what all this clusterfuck is doing.

First off, the list of services exists in *services*. Each service
has either a :before or :after slot on it, which explains what's
run before this service, or what needs to be run after this service.

Based on that, there is a DAG. The services themselves are not really
suited to the order stuff though, because the information is spread
out everywhere. So first off, a proper DAG is built, based on
graph-element objects, that hold a reference to the service. While
doing that, the elements without any parent (aka roots) are set aside.

Once the graph-elements are all created, linit is going to check
for circular references using Tarjan's algorithm. If any cycle is
found, then the whole path is cancelled.
If linit didn't cancel, then there would be some infinite loop
between the 2 circular dependencies, so it's kind of mandatory.
What's nice is that the debug message is useful. Despite cancelling
the whole path, if the other paths are fine, they'll be run.

Then, the roots are started, and then their children, etc.

Every time a service is started, it's going to check if all its
dependencies are started before. If any is missing, then it's
cancelling its start. When the last dependency will start, the
next tick will call the child, which will check all of its deps
and be fine, so it'll start. This bit is fairly easy."
  (let ((graph (make-dag services)))
    (dolist (el (root-elements graph))
      (unless (has-cycle el)
        (start-graph-services el)))))

(defun start-service (service)
  (format t "Starting ~A...~%" (name service))
  (let ((pid (sb-posix:fork)))
    (cond
      ((<= pid -1) (progn
                     (setf (state service) 'errored)
                     (format t "Error starting ~A~%" (name service))))
      ((= pid 0) (sb-ext:exit
                  :code (handler-case
                            (let ((startfn (start service)))
                              (when startfn
                                (let ((*package* (find-package "LINIT-USER")))
                                  (funcall startfn)))
                              0)
                          (error () -1))))
      (t (multiple-value-bind (state presentp)
             (gethash pid *sigchld-pids*)
           (if presentp
               ;; This pid has already stopped or errored,
               ;; so we shouldn't set it to "started".
               (progn
                 (remhash pid *sigchld-pids*)
                 (setf (pid service) pid
                       (state service) state))
               (progn
                 (setf (pid service) pid
                       (state service) 'started)
                 (format t "~A started.~%" (name service)))))))))
