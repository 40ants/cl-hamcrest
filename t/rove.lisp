(uiop:define-package #:hamcrest-tests/rove
  (:use #:cl
        #:rove
        #:hamcrest/rove
        #:hamcrest/utils)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:rove/core/assertion)
  (:import-from #:hamcrest/matchers
                #:assertion-error
                #:assertion-error-reason)
  (:import-from #:cl-ppcre
                #:scan))
(in-package #:hamcrest-tests/rove)


(defmacro test-assertion (title body expected &key (regexp t))
  "Tests that assertion result in prove's output
matches given regular expression.

Body evaluated and it's result is matched agains expected string,
using ppcre:scan. Dangling spaces and newlines are trimmed from
the result before trying to match."
  
  (with-gensyms (result trimmed-result)
    (let* ((deindented-expected (deindent expected))
           (assertion-form
             (if regexp
                 `(ok (not (null (scan ,deindented-expected
                                       ,trimmed-result)))
                      (format nil "~S matches ~S regex"
                              ,trimmed-result
                              ,deindented-expected))
                 `(ok (equal ,deindented-expected
                             ,trimmed-result)))))
      `(testing ,title
         (let* ((,result
                  ;; All output during the test, should be captured
                  ;; to test against give regex
                  ;; 
                  ;; We need to bind rove:*stats* to a new reporter here.
                  ;; Otherwise, checked assertion's result will be included
                  ;; in the hamcrest-test test run results, but we don't
                  ;; want it.
                  (with-reporter :spec
                    ;; Here we catch output of a single assertion
                    (rove/core/assertion::output-of
                        ;; Also, we need to turn of coloring
                        ;; because terminal escape characters
                        ;; will prevent text matching when running
                        ;; tests in console or on CI
                        (let ((rove:*enable-colors* nil))
                          (handler-case
                              (progn ,body)
                            (hamcrest/matchers:assertion-error ()))))))

                (,trimmed-result
                  (string-trim '(#\Space #\Newline)
                               (deindent ,result))))

           ,assertion-form)))))


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
     "✓ Contains all given values.*")

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
× 0\\) Item with index 1
    Key :AGE is missing.*")))


(deftest implicit-has-all
  "Assert-that should use implicit has-all, to combine multiple matchers"
  
  (let ((value '(:name "Irina")))
    (test-assertion
     "Check if object have :name key, but not :husband key"
     (assert-that value
                  (has-plist-entries :name "Irina")
                  (hasnt-plist-keys :husband))
     "✓ All checks are passed")))


(deftest implicit-has-all-failed
  "Assert-that should use implicit has-all, to combine multiple matchers"
  
  (let ((value '(:name "Irina")))
    (test-assertion
     "Check if object have :name key, and :husband key"
     (assert-that value
                  (has-plist-entries :name "Irina")
                  (has-plist-entries :husband t))
     "× 0) Key :HUSBAND is missing"
     :regexp nil)))

