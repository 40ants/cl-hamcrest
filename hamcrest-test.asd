(defsystem hamcrest-test
  :version (:read-file-form "version.lisp-expr")
  :author "Alexander Artemenko"
  :license "New BSD License"
  :depends-on (:hamcrest-prove
               :prove)
  :components ((:module "t"
                        :components
                        ((:test-file "matchers")
                         (:test-file "prove")
                         (:test-file "utils"))))
  :description "Test system for cl-hamcrest."

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (symbol-call :prove-asdf :run-test-system c)
                    (clear-system c)))
