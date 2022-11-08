(uiop:define-package #:hamcrest-tests/prove
  (:use #:cl
        #:rove
        #:hamcrest/prove
        #:hamcrest/utils)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:hamcrest/matchers
                #:assertion-error
                #:assertion-error-reason)
  (:import-from #:cl-ppcre
                #:scan))
(in-package #:hamcrest-tests/prove)


(defmacro test-assertion (title body expected)
  "Tests that assertion result in prove's output
matches given regular expression.

Body evaluated and it's result is matched agains expected string,
using ppcre:scan. Dangling spaces and newlines are trimmed from
the result before trying to match."
  
  (with-gensyms (result trimmed)
    `(testing ,title
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
                        (prove.suite:*suite* (make-instance 'prove.suite:suite))
                        (prove.reporter::*debug-indentation* nil))
                    ,body)))

              (,trimmed (string-trim '(#\Space #\Newline)
                                     (deindent ,result))))
         (ok (not (null (scan ,trimmed
                              (deindent ,expected)))))))))


(deftest nested-matchers
  "Checking nested matchers in Prove's assert-that"
  
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
     "
× Item with index 1
    Key :AGE is missing")))


(deftest implicit-has-all
  "Assert-that should use implicit has-all, to combine multiple matchers"
  
  (let ((value '(:name "Irina")))
    (test-assertion
     "Check if object have :name key, but not :husband key"
     (assert-that value
                  (has-plist-entries :name "Irina")
                  (hasnt-plist-keys :husband))
     "✓ All checks are passed")))
