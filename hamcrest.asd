#-asdf3.1 (error "cl-info requires ASDF 3.1")
(defsystem hamcrest
  :version (:read-file-form "version.lisp-expr")
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "New BSD License"
  :pathname "src"
  :depends-on ("hamcrest/utils"
               "hamcrest/matchers")
  :description "A set of helpers to make your unittests more readable by using Hamcrest assertions."
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
