(defpackage oop
  (:use :cl)
  (:export #:create
           #:extend
           #:get-property
           #:set-property
           #:call))
(in-package :oop)

(defun create (&rest props)
  (lambda (prop &optional (setter nil) (val nil))
    (when setter
      (setf (getf props prop) val))
    (getf props prop)))

(defun extend (base &rest props)
  (let ((props (append `(:base ,base) props)))
    (lambda (prop &optional (setter nil) (val nil))
      (when setter
        (setf (getf props prop) val))

      (if (find prop props)
        (getf props prop)
        (get-property base prop)))))

(defun get-property (object property)
  (apply object `(,property)))

(defun set-property (object property val)
  (apply object `(,property ,t ,val)))

(defun call (obj f &rest args)
  (apply (get-property obj f) (append `(,obj) args)))
