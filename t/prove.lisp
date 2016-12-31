(in-package :cl-user)
(defpackage hamcrest.t.prove
  (:use :cl
        :prove
        :hamcrest.prove)
  (:import-from :alexandria
                :with-gensyms))
(in-package :hamcrest.t.prove)


(plan 3)


(defmacro test-assertion (title body expected)
  "Tests that assertion result in prove's output
matches given regular expression.

Body evaluated and it's result is matched agains expected string,
using prove:like. Dangling spaces and newlines are trimmed from
the result before trying to match."
  
  (with-gensyms (result trimmed)
    `(subtest ,title
       (let* (;; We need to overide current suite, to prevent
              ;; tested assert-that macro from modifying real testsuite.
              ;; Otherwise it can increment failed or success tests count
              ;; and prove will output wrong data.
              (prove.suite:*suite* (make-instance 'prove.suite:package-suite))
              ;; All output during the test, should be captured
              ;; to test against give regex
              (,result (with-output-to-string
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

    (test-assertion
     "Placeholder _ can match any value"
     (assert-that value
       (has-alist-entries
        :bar _))
     "✓ Has alist entries \\(:BAR _\\)")))


(subtest "'Any' matcher  and placeholder"
  (test-assertion
   "'Any' matcher matches any value"
   (assert-that 1 (any))
   "✓ Any value if good enough")

  (test-assertion
   "'Any' matcher can be replaced with '_' placeholder"
   (assert-that 1 _)
   "✓ Any value if good enough"))


(subtest "Contains matcher"
  (test-assertion
   "Good scenario"
   (assert-that '(1 :two "three")
                (contains 1 :two "three"))
   "✓ Contains all given values")

  (test-assertion
   "Bad scenario, when value is shorter"
   (assert-that '(1)
                (contains 1 :two "three"))
   "× Result is shorter than expected")

  (test-assertion
   "Bad scenario, when expected value is shorter"
   (assert-that '(1 :two "three")
                (contains 1))
   "× Expected value is shorter than result")

  (test-assertion
   "Bad scenario, when some item mismatch"
   (assert-that '(1 "two" "three")
                (contains 1 :two "three"))
   "× Item \"two\" at index 1, but :TWO was expected"))

(finalize)
