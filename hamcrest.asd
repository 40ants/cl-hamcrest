(in-package :cl-user)
(defpackage hamcrest-asd
  (:use :cl :asdf))
(in-package :hamcrest-asd)


(defsystem hamcrest
  :version "0.1.0"
  :author "Alexander Artemenko"
  :license "BSD"
  :depends-on (:iterate
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "matchers"))))
  :description "A set of helpers to make your unittests more readable."
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


(defsystem hamcrest-prove
  :version "0.1.0"
  :author "Alexander Artemenko"
  :license "BSD"
  :depends-on (:iterate
               :hamcrest
               :prove)
  :components ((:module "src"
                :components
                ((:file "prove"))))
  :description "A set of helpers to make your Prove unittests more readable."
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
