(in-package :cl-user)
(defpackage hamcrest.t.prove
  (:use :cl
        :prove
        :hamcrest.prove))
(in-package :hamcrest.t.prove)


(plan nil)

(subtest "Just plain prove test"
  (is "Blah" "Blah")
  (is "Blah" t))

(subtest
    "Alist assertions"
  (let ((value '((:foo . 1)
                 (:bar . 2))))
    (subtest
        "Successful match"
      (assert-that value
                   (has-alist-entries
                    :foo 1
                    :bar 2)))

    (subtest
        "Missing value"
      (assert-that value
                   (has-alist-entries
                    :baz 1)))

    (subtest
        "Ignored value"
      (assert-that value
                   (has-alist-entries
                    :baz _)))))

(finalize)
