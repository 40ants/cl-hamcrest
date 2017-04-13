(in-package :cl-user)
(defpackage hamcrest-test-asd
  (:use :cl :asdf))
(in-package :hamcrest-test-asd)


(defsystem hamcrest-test
  :version (:read-file-form "version.lisp-expr")
  :author "Alexander Artemenko"
  :license "BSD"
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
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
