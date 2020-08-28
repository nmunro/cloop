(defpackage oop/tests/main
  (:use :cl
        :oop
        :rove))
(in-package :oop/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :oop)' in your Lisp.

(deftest test-basic-properties-1
  (testing "Object can get a name"
    (let ((person (build :name "Gary")))
      (ok (string= "Gary" (get-property person :name))))))

(deftest test-basic-properties-2
  (testing "Object can get a name and an age"
    (let ((person (build :name "Gary" :age 24)))
      (ok (= 24 (get-property person :age))))))

(deftest test-basic-properties-3
  (testing "Object can access multiple properties"
    (let ((person (build :name "Gary" :age 24)))
      (ok (string= "Gary is 24" (format nil "~A is ~A" (get-property person :name) (get-property person :age)))))))

(deftest test-setters-1
  (testing "Object setter returns new value"
    (let ((person (build :name "Gary" :age 24)))
      (ok (string= "Bob" (set-property person :name "Bob"))))))

(deftest test-setters-2
  (testing "Extended Object setter returns new value"
    (let* ((person1 (build :name "Gary" :age 24))
           (person2 (extend person1 :siblings t)))
      (ok (string= "Bob" (set-property person2 :name "Bob"))))))

(deftest test-properties-can-be-changed-1
  (testing "Object can change the value of the name property"
    (let ((person (build :name "Gary" :age 24)))
      (set-property person :name "Bob")
      (ok (string= "Bob" (get-property person :name))))))

(deftest test-properties-can-be-changed-2
  (testing "Object can change the value of the age property"
    (let ((person (build :name "Gary" :age 24)))
      (set-property person :age 26)
      (ok (= 26 (get-property person :age))))))

(deftest test-properties-can-be-nil
  (testing "An object that has a property value of nil doesn't look up the chain"
    (let* ((person1 (build :name "Gary" :age 24))
           (person2 (extend person1 :name nil)))
      (ok (and (string= (get-property person1 :name) "Gary") (eq (get-property person2 :name) nil))))))

(deftest test-inheritence-1
  (testing "Object can inherit from another object"
    (let* ((person1 (build :name "Gary" :age 24))
           (person2 (extend person1 :siblings t)))
      (ok (and (= 24 (get-property person2 :age))
               (string= "Gary" (get-property person2 :name))
               (eq t (get-property person2 :siblings))
               (eq nil (get-property person1 :siblings)))))))

(deftest test-inheritence-2
  (testing "Object can shadow property names"
    (let* ((person1 (build :name "Gary" :age 24))
           (person2 (extend person1 :name "Bob")))
      (ok (string= "Bob" (get-property person2 :name))))))

(deftest test-inheritence-3
  (testing "Object can expose its parent"
    (let* ((o1 (build :name "NMunro" :age 33 :can-vote (lambda (this) (>= (get-property this :age) 18))))
           (o2 (extend o1 :name "Gary" :greeting "Hello!" :say-hi (lambda (this) (format nil "~A" (get-property this :greeting))))))
    (ok (string= (get-property (super o2) :name) "NMunro")))))

(deftest test-inheritence-4
  (testing "Object can extend from an extended object"
    (let* ((o1 (build :name "NMunro" :age 33 :can-vote (lambda (this) (>= (get-property this :age) 18))))
           (o2 (extend o1 :name "Gary" :greeting "Hello!" :say-hi (lambda (this) (format nil "~A" (get-property this :greeting)))))
           (o3 (extend o2 :name "Bob")))
    (ok (string= (get-property (super o3) :name) "Gary")))))


(deftest test-methods-1
  (testing "Methods can be used via apply"
    (let ((person (build :name "Gary" :age 24 :can-vote (lambda (this) (>= (get-property this :age) 18)))))
      (ok (apply (get-property person :can-vote) `(,person))))))

(deftest test-methods-2
  (testing "Methods can be used via 'call'"
    (let ((person (build :name "Gary" :age 24 :can-vote (lambda (this) (>= (get-property this :age) 18)))))
      (ok (call person :can-vote)))))

(deftest test-methods-3
  (testing "Methods on the 'super' can be used via 'call'"
           (let* ((p1 (build :name "Gary" :age 24 :can-vote (lambda (this) (>= (get-property this :age) 18))))
                  (p2 (extend p1 :name "Bob" :age 17)))
      (ok (and (eq t (call (super p2) :can-vote)) (eq nil (call p2 :can-vote)))))))
