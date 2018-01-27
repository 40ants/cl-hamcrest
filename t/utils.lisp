(in-package :cl-user)
(defpackage hamcrest.t.utils
  (:use :cl
        :prove
        :hamcrest/src/utils))
(in-package :hamcrest.t.utils)


(plan 2)

(subtest
    "Checking alistp predicate"
  (let ((good-values (list nil
                           '((:foo . 1)
                             (:bar . 2))))
        (bad-values (list 100500
                          "Blah"
                          '((:foo . 1)
                            :bar 2)
                          #(#(:foo 1)
                            #(:bar 2)))))
    (loop
       :for value
       :in good-values
       :do (is (alistp value)
               t
               (format nil "~S is a proper alist" value)))

    (loop
       :for value
       :in bad-values
       :do (is (alistp value)
               nil
               (format nil "~S is not a proper alist" value)))))


(subtest "Deindent text"
  (let ((text "
    Blah
      Minor
        - again;
        - and again.
")
        (expected "Blah
  Minor
    - again;
    - and again."))
    (is (deindent text) expected)))


(finalize)
