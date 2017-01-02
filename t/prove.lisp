(in-package :cl-user)
(defpackage hamcrest.t.prove
  (:use :cl
        :prove
        :hamcrest.prove)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :hamcrest.matchers
                :assertion-error
                :assertion-error-reason))
(in-package :hamcrest.t.prove)


(plan 1)


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

                 (let (;; Colors whould be turned off to
                       ;; prevent Prove's reporter return
                       ;; string with terminal sequences.
                       ;; This way it will be easier to compare
                       ;; results with usual strings
                       (prove.color:*enable-colors* nil)
                       ;; We need to overide current suite, to prevent
                       ;; tested assert-that macro from modifying real testsuite.
                       ;; Otherwise it can increment failed or success tests count
                       ;; and prove will output wrong data.
                       (prove.suite:*suite* (make-instance 'prove.suite:suite)))
                   ,body)))

              (,trimmed (string-trim '(#\Space #\Newline)
                                     ,result)))
         (like ,trimmed ,expected)))))


(subtest "Nested matchers in Prove's assert-that"
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
                   ;; but :age key is absent
                   (has-alist-entries :age 40)))
     ;; Here matcher should show full context with
     ;; description of all  higher level matchers,
     ;; like:
     ;;
     ;; Second item:
     ;;   Key AGE is missing
     "× Item with index 1
  Key :AGE is missing")))


(finalize)
