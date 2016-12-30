(in-package :cl-user)
(defpackage hamcrest.t.prove
  (:use :cl
        :prove
        :hamcrest.prove)
  (:import-from :alexandria
                :with-gensyms))
(in-package :hamcrest.t.prove)


(plan nil)


(subtest "Just plain prove test"
  (is "Blah" "Blah")
  (is t t))


(subtest "Just plain prove test with failed assertion"
  (is "Blah" t))


(defmacro test-assertion (title body expected)
  "Tests that assertion result in prove's output
matches given regular expression.

Body evaluated and it's result is matched agains expected string,
using prove:like. Dangling spaces and newlines are trimmed from
the result before trying to match."
  
  (with-gensyms (result trimmed)
    `(subtest ,title
       (let* ((,result (with-output-to-string
                           (prove.output:*test-result-output*)
                         ,body))
              (,trimmed (string-trim '(#\Space #\Newline)
                                     ,result)))
         (like ,trimmed ,expected)))))


(subtest
    "Alist assertions"
  (let ((value '((:foo . 1)
                 (:bar . 2))))

    (test-assertion
     "Successful match"
     (assert-that value
                  (has-alist-entries
                   :foo 1
                   :bar 2))
     "✓ Has alist entries \\(:FOO 1 :BAR 2\\)")

    (test-assertion
        "Missing value"
      (assert-that value
                   (has-alist-entries
                    :baz 1))
      "× Key BAZ is missing")

    (subtest
        "Ignored value"
      (assert-that value
                   (has-alist-entries
                    :baz _)))))

(finalize)
