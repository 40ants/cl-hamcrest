#-asdf3.1 (error "cl-info requires ASDF 3.1")
(defsystem "hamcrest-tests"
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "New BSD License"
  :pathname "t"
  :depends-on ("hamcrest-tests/prove"
               "hamcrest-tests/rove"
               "hamcrest-tests/matchers"
               "hamcrest-tests/utils")
  :description "Test system for cl-hamcrest."

  :perform (test-op :after (op c)
                    (symbol-call :rove :run c)))
