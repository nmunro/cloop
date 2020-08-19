(defpackage oop
  (:use :cl)
  (:export #:build
           #:extend
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
  (let ((props (append `(:super ,(lambda (self) base)) props)))
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

(let* ((p1 (build :name "NMunro" :age 2345 :hi (lambda (self) (format nil "Hello world!~%"))))
       (p2 (extend p1 :name "Bob" :age 18)))
  (format nil "~A inherits from ~A~%" (get-property p2 :name) (get-property (call p2 :super) :name)))
