(in-package :cl-user)
(defpackage hamcrest.t.matchers
  (:use :cl
        :prove
        :hamcrest.matchers)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :hamcrest.matchers
                :assertion-error
                :assertion-error-reason
                :assertion-error-reason-with-context))
(in-package :hamcrest.t.matchers)


(plan 5)


(defmacro test-if-matcher-fails (title matcher value expected-error-message)
  `(subtest ,title
     (is-condition
      (funcall ,matcher
               ,value)
      assertion-error
      (list :test (lambda (got expected)
                    (when (typep got expected)
                      (let ((reason (assertion-error-reason-with-context got)))
                        ;; if there is .* in expected message, then
                        ;; we'll choose to use "like" macro for checking
                        ;; assertion reason against expected value
                        (,(if (search ".*" expected-error-message)
                              'like
                              'is)
                          reason ,expected-error-message
                          "Condition should have correct error message"))))
            "Matcher should signal ASSERTION-ERROR condition"))))


(defmacro test-if-matcher-ok (title matcher value expected-matcher-docstring)
  `(subtest ,title
     (let ((matcher ,matcher))
       (ok
        (funcall ,matcher
                 ,value)
        "Matcher should return t.")
       (is (documentation matcher 'function)
           ,expected-matcher-docstring
           (format nil "Matcher's docstring should be \"~a\"."
                   ,expected-matcher-docstring)))))


(subtest
    "Alist assertions"
  (let ((value '((:foo . 1)
                 (:bar . 2)))
        (not-alist '(:foo 1 :bar 2)))

    (test-if-matcher-ok
     "Successful match"
     (has-alist-entries :foo 1 :bar 2)
     value
     "Has alist entries (:FOO 1 :BAR 2)")

    (test-if-matcher-fails
     "Missing value"
     (has-alist-entries :baz 1)
     value
     "Key :BAZ is missing")

    (test-if-matcher-ok
     "Placeholder _ can match any value"
     (has-alist-entries :bar _)
     value
     "Has alist entries (:BAR _)")

    (test-if-matcher-fails
     "Checked value should be proper alist"
     (has-alist-entries :foo 1 :bar 2)
     not-alist
     "Value is not alist")))


(subtest
    "Plist assertions"
  (let ((value '(:foo 1
                 :bar 2))
        (not-list 1))

    (test-if-matcher-ok
     "Successful match"
     (has-plist-entries :foo 1 :bar 2)
     value
     "Has plist entries (:FOO 1 :BAR 2)")

    (test-if-matcher-fails
     "Missing value"
     (has-plist-entries :baz 1)
     value
     "Key :BAZ is missing")

    (test-if-matcher-ok
     "Placeholder _ can match any value"
     (has-plist-entries :bar _)
     value
     "Has plist entries (:BAR _)")

    (test-if-matcher-fails
     "Checked value should be a list"
     (has-plist-entries :foo 1 :bar 2)
     not-list
     "Value is not a list")))


(subtest "'Any' matcher  and placeholder"
  (test-if-matcher-ok
   "'Any' matcher matches any value"
   (any)
   1
   "Any value if good enough"))


(subtest "Contains matcher"
  (test-if-matcher-ok
   "Good scenario"
   (contains 1 :two "three")
   '(1 :two "three")
   "Contains all given values")

  (test-if-matcher-fails
   "Bad scenario, when value is shorter"
   (contains 1 :two "three")
   '(1)
   "Result is shorter than expected")

  (test-if-matcher-fails
   "Bad scenario, when expected value is shorter"
   (contains 1)
   '(1 :two "three")
   "Expected value is shorter than result")

  (test-if-matcher-fails
   "Bad scenario, when some item mismatch"
   (contains 1 :two "three")
   '(1 "two" "three")
   "Item \"two\" at index 1, but :TWO was expected")

  (test-if-matcher-ok
   "Good scenario, with some placeholders"
   (contains 1 _ "three")
   '(1 :two "three")
   "Contains all given values")

  (test-if-matcher-ok
   "Good scenario, with another placeholders"
   (contains _ _ "three")
   '(1 :two "three")
   "Contains all given values")

  (let ((value '(((:name . "Maria"))
                 ((:name . "Alexander")))))
    (test-if-matcher-ok
     "Check if all alists in the list have :name entry"
     (contains
      ;; we need exactly this value in the first object
      (has-alist-entries :name "Maria")
      ;; and we don't care about name of the second person
      (has-alist-entries :name _))
     value
     "Contains all given values")

    (test-if-matcher-fails
     "Check if some nested matcher will fail"
     (contains
      ;; everything is ok here
      (has-alist-entries :name "Maria")
      ;; but :age key is absent
      (has-alist-entries :age 40))
     value
     ;; Here matcher should show full context with
     ;; description of all  higher level matchers,
     ;; like:
     ;;
     ;; Second item:
     ;;   Key AGE is missing
     "Item with index 1
  Key :AGE is missing")))


(subtest "Contains in any order"
  (let ((value (list 4 5 3 1))
        (complex (list 3 2 '((:foo "bar")) 1)))
    (test-if-matcher-ok
     "Works with list and don't modifies it"
     (contains-in-any-order 1 4 3 5)
     value
     "Contains all given values")
    ;; now check that original value does not touched
    (is value (list 4 5 3 1))

    (test-if-matcher-fails
     "And fails if some item not found in given list"
     (contains-in-any-order 1 4 2 5)
     value
     "Value 2 is missing")

    (test-if-matcher-fails
     "And fails if some complex item not found in given list"
     (contains-in-any-order
      1 2 3
      (has-alist-entries :foo "bar"))
     complex
     "Value which \"Has alist entries .*\" is missing")))


(finalize)
