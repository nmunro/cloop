(defpackage oop
  (:use :cl)
  (:export #:build
           #:extend
           #:super
           #:get-property
           #:set-property
           #:call))
(in-package :oop)

(defun build (&rest props)
  (lambda (prop &optional (setter nil) (val nil))
    (when setter
      (setf (getf props prop) val))
    (getf props prop)))

(defun extend (base &rest props)
  (let ((props (append `(:super ,base) props)))
    (lambda (prop &optional (setter nil) (val nil))
      (when setter
        (setf (getf props prop) val))

      (if (find prop props)
        (getf props prop)
        (apply base `(,prop))))))

(defun get-property (object property)
  (apply object `(,property)))

(defun set-property (object property val)
  (apply object `(,property ,t ,val)))

(defun call (obj f &rest args)
  (apply (apply obj `(,f)) (append `(,obj) args)))

(defun super (obj)
  (apply obj `(:super)))
