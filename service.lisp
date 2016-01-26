(in-package #:linit)

(defvar *services* nil)
(defvar *graph-elements* nil)

(deftype service-state ()
  '(member started stopped errored))

(defclass service ()
  ((pid :accessor pid :type integer)
   (state :accessor state :type service-state :initform nil)
   (name :reader name :initarg :name :type symbol)
   (start :initarg :start :reader start :type function :initform nil)
   (before :initarg :before :reader before :type list :initform nil)
   (after :initarg :after :reader after :type list :initform nil)))

(defclass graph-element ()
  ((service :initarg :service :reader service :type service)
   (mark :accessor mark :type boolean :initform nil)
   (children :accessor children
             :type (vector service)
             :initform (make-array 0
                                   :element-type 'service
                                   :adjustable t
                                   :fill-pointer 0))
   (parents :accessor parents
            :type (vector service)
            :initform (make-array 0
                                  :element-type 'service
                                  :adjustable t
                                  :fill-pointer 0))))

(defmacro defservice (name &rest initargs)
  (let ((new-service (gensym)))
    `(let ((,new-service (make-instance 'service
                                       :name ',name
                                       ,@initargs)))
       (if (find-service ',name)
           (replace-service ,new-service)
           (add-service ,new-service)))))

(defun find-service (name)
  (find-if (lambda (service)
             (eq (name service) name))
           *services*))

(defun find-service-by-pid (pid)
  (find-if (lambda (service)
             (eq (pid service) pid))
           *services*))

(defun replace-service (new-service)
  (setf *services* (append
                    (remove-if (lambda (service)
                                 (eq (name service) (name new-service)))
                               *services*)
                    new-service)))

(defun add-service (service)
  (push service *services*))

(defun load-services (path)
  (dolist (service (directory path))
    ;; For some reason, this needs to be re-applied for the load'ed
    ;; file to be in the correct package.
    (in-package #:linit)
    (load service)))

(defun start-services ()
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
  (let ((graph (make-dag *services*)))
    (dolist (el (root-elements graph))
      (unless (has-cycle el)
        (start-graph-services el)))))

(defun root-elements (elements)
  (remove-if-not (lambda (element)
                   (= 0 (length (parents element))))
                 elements))

(defun has-cycle (el)
  (when (mark el)
    (return-from has-cycle t))
  (setf (mark el) t)
  (loop for child across (children el)
     do (when (has-cycle child)
          (return-from has-cycle t)))
  (setf (mark el) nil))

(defun start-graph-services (el)
  (when (every (lambda (parent)
                 ;; aka non-nil
                 (state (service parent)))
               (parents el))
    (start-service (service el))
    (loop for child across (children el) do (start-graph-services child))))

(defun make-dag (services)
  (let ((dag (mapcar (lambda (service)
                       (make-instance 'graph-element
                                      :service service))
                     services)))
    (multiple-value-prog1 dag
      (dolist (el dag)
       (when (before (service el))
         ;; This element is the child of another
         (dolist (parent-symbol (before (service el)))
           (let ((parent (find-graph-element-by-service-name dag parent-symbol)))
             (vector-push-extend el (children parent))
             (vector-push-extend parent (parents el)))))
       (when (after (service el))
         ;; This element is the parent of another
         (dolist (child-symbol (after (service el)))
           (let ((child (find-graph-element-by-service-name dag child-symbol)))
             (vector-push-extend child (children el))
             (vector-push-extend el (parents child)))))))))

(defun find-graph-element-by-service-name (dag name)
  (find-if (lambda (el)
             (eq (name (service el)) name))
           dag))

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
                                (funcall startfn))
                              0)
                          (error () -1))))
      (t (progn
           (setf (pid service) pid
                 (state service) 'started)
           (format t "~A started.~%" (name service)))))))
