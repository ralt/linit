(in-package #:linit)

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

(defun has-stopped-requirement (el parent)
  (or
   (and
    (before-stopped (service el))
    (member (name (service parent))
            (before-stopped (service el))))
   (and
    (after-stopped (service parent))
    (member (name (service el))
            (after-stopped (service parent))))))

(defun start-graph-services (el)
  (format t "Maybe starting ~A~%" (name (service el)))
  (when (and
         ;; Don't start services that already started
         (null (state (service el)))
         (every (lambda (parent)
                  (if (has-stopped-requirement el parent)
                      (eq (state (service parent)) 'stopped)
                      ;; aka non-nil
                      (state (service parent))))
                (parents el)))
    (start-service (service el))
    (loop for child across (children el) do (start-graph-services child))))

(defun make-dag (services)
  (let ((dag (mapcar (lambda (service)
                       (make-instance 'graph-element
                                      :service service))
                     services)))
    (mapcar
     (lambda (el)
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
             (vector-push-extend el (parents child)))))
       el)
     dag)))

(defun find-graph-element-by-service-name (dag name)
  (find-if (lambda (el)
             (eq (name (service el)) name))
           dag))
