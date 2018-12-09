#-asdf3.1 (error "cl-info requires ASDF 3.1")
(defsystem hamcrest-test
  :version (:read-file-form "version.lisp-expr")
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "New BSD License"
  :pathname "t"
  :depends-on ("hamcrest-test/prove"
               "hamcrest-test/rove"
               "hamcrest-test/matchers"
               "hamcrest-test/utils")
  :description "Test system for cl-hamcrest."

  :perform (test-op :after (op c)
                    (symbol-call :rove :run c)))
