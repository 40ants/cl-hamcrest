(in-package :cl-user)
(defpackage hamcrest-prove-asd
  (:use :cl :asdf))
(in-package :hamcrest-prove-asd)


(defsystem hamcrest-prove
  :version (:read-file-form "version.lisp-expr")
  :author "Alexander Artemenko"
  :license "New BSD License"
  :depends-on (:iterate
               :prove
               :hamcrest)
  :components ((:module "src"
                :components
                ((:file "prove"))))
  :description "A set of helpers to make your Prove unittests more readable by using Hamcrest assertions."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.rst"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq)
                (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op hamcrest-test))))
