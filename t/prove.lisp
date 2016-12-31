(in-package :cl-user)
(defpackage hamcrest.t.prove
  (:use :cl
        :prove
        :hamcrest.prove)
  (:import-from :alexandria
                :with-gensyms))
(in-package :hamcrest.t.prove)


(plan 4)


(defmacro test-assertion (title body expected)
  "Tests that assertion result in prove's output
matches given regular expression.

Body evaluated and it's result is matched agains expected string,
using prove:like. Dangling spaces and newlines are trimmed from
the result before trying to match."
  
  (with-gensyms (result trimmed)
    `(subtest ,title
       (let* ((,result
               ;; All output during the test, should be captured
               ;; to test against give regex
               (with-output-to-string
                   (prove.output:*test-result-output*)

                 ;; We need to overide current suite, to prevent
                 ;; tested assert-that macro from modifying real testsuite.
                 ;; Otherwise it can increment failed or success tests count
                 ;; and prove will output wrong data.
                 (let ((prove.suite:*suite* (make-instance 'prove.suite:suite)))
                   ,body)))

              (,trimmed (string-trim '(#\Space #\Newline)
                                     ,result)))
         (like ,trimmed ,expected)))))


(subtest
    "Alist assertions"
  (let ((value '((:foo . 1)
                 (:bar . 2)))
        (not-alist '(:foo 1 :bar 2)))

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
     "× Key :BAZ is missing")

    (test-assertion
     "Placeholder _ can match any value"
     (assert-that value
                  (has-alist-entries
                   :bar _))
     "✓ Has alist entries \\(:BAR _\\)")

    (test-assertion
     "Checked value should be proper alist"
     (assert-that not-alist
                  (has-alist-entries
                   :foo 1
                   :bar 2))
     "× Value is not alist")))


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
   "× Item \"two\" at index 1, but :TWO was expected")

  (test-assertion
   "Good scenario, with some placeholders"
   (assert-that '(1 :two "three")
                (contains 1 _ "three"))
   "✓ Contains all given values")

  (test-assertion
   "Good scenario, with another placeholders"
   (assert-that '(1 :two "three")
                (contains _ _ "three"))
   "✓ Contains all given values")

  (let ((value '(((:name . "Maria"))
                 ((:name . "Alexander")))))
    (test-assertion
     "Check if all alists in the list have :name entry"
     (assert-that value
                  (contains
                   ;; we need exactly this value in the first object
                   (has-alist-entries :name "Maria")
                   ;; and we don't care about name of the second person
                   (has-alist-entries :name _)))
     "✓ Contains all given values")

    (test-assertion
     "Check if some nested matcher will fail"
     (assert-that value
                  (contains
                   ;; everything is ok here
                   (has-alist-entries :name "Maria")
                   (has-alist-entries :age 40)))
     ;; TODO: actually, what I want here is the context,
     ;;       like:
     ;;       Key AGE is missing in the second item.
     ;;       or
     ;;       Second item:
     ;;         Key AGE is missing
     "× Key :AGE is missing")))


(subtest "Contains in any order"
  (let ((value (list 4 5 3 1))
        (complex (list 3 2 '((:foo "bar")) 1)))
    (test-assertion
     "Works with list and don't modifies it"
     (assert-that value
                  (contains-in-any-order 1 4 3 5))
     "✓ Contains all given values")
    ;; now check that original value does not touched
    (is value (list 4 5 3 1))

    (test-assertion
     "And fails if some item not found in given list"
     (assert-that value
                  ;; value 2 is absent in checked list
                  (contains-in-any-order 1 4 2 5))
     "× Value 2 is missing")

    (test-assertion
     "And fails if some complex item not found in given list"
     (assert-that complex
                  (contains-in-any-order
                   1 2 3
                   (has-alist-entries :foo "bar")))
     "× Value which \"Has alist entries .*\" is missing")))


(finalize)
