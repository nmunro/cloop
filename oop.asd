(defsystem "oop"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "oop/tests"))))

(defsystem "oop/tests"
  :author ""
  :license ""
  :depends-on ("oop"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for oop"
  :perform (test-op (op c) (symbol-call :rove :run c)))
