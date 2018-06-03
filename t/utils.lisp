(defpackage hamcrest-test/utils
  (:use :cl
        :rove
        :hamcrest/utils))
(in-package hamcrest-test/utils)


(deftest test-alistp-predicate
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
       :do (ok (equal (alistp value)
                      t)
               (format nil "~S is a proper alist" value)))

    (loop
       :for value
       :in bad-values
       :do (ok (null (alistp value))
               (format nil "~S is not a proper alist" value)))))


(deftest deindent-text
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
    (ok (equal (deindent text)
               expected))))

