(defpackage hamcrest-test/matchers
  (:use :cl
        :rove
        :hamcrest/matchers)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :hamcrest/matchers
                :assertion-error
                :assertion-error-reason
                :assertion-error-reason-with-context))
(in-package hamcrest-test/matchers)


(defmacro test-if-matcher-fails (title matcher value expected-error-message)
  "This macro generates a test which checks that
   matcher applied to the given value will signal assertion-error
   and it's error message will match to expected-error-message."
  `(testing ,title
     (let* ((condition nil)
            (result (handler-case (funcall ,matcher
                                           ,value)
                      (assertion-error (c)
                        (setf condition c))))
            (reason (when condition
                      (assertion-error-reason-with-context condition))))
       ;; We don't interested in the matching result
       ;; because here we are checking if a condition was signaled
       (declare (ignorable result))
       
       (ok (and condition reason)
           "Matcher should signal ASSERTION-ERROR condition")
       
       (ok (if (search ".*" ,expected-error-message)
               (cl-ppcre:scan ,expected-error-message
                              reason)
               (equal reason
                      ,expected-error-message))
           "Condition should have correct error message"))))


(defmacro test-if-matcher-ok (title matcher value expected-matcher-docstring)
  (with-gensyms (matcher-var matcher-description)
    `(testing ,title
       (let* ((,matcher-var ,matcher)
              (,matcher-description (matcher-description ,matcher-var)))
         (ok
          (funcall ,matcher-var
                   ,value)
          "Matcher should return t.")
         (ok (equal ,matcher-description
                    ,expected-matcher-docstring)
             (format nil "Matcher description should be:~%\"~a\"."
                     ,expected-matcher-docstring))))))


(deftest alist-assertions
  (let ((value '((:foo . 1)
                 (:bar . 2)))
        (not-alist '(:foo 1 :bar 2)))

    (test-if-matcher-ok
     "Successful match"
     (has-alist-entries :foo 1 :bar 2)
     value
     "Has alist entries:
  :FOO = 1
  :BAR = 2")

    (test-if-matcher-fails
     "Missing value"
     (has-alist-entries :baz 1)
     value
     "Key :BAZ is missing")

    (test-if-matcher-ok
     "Placeholder _ can match any value"
     (has-alist-entries :bar _)
     value
     "Has alist entries:
  :BAR = _")

    (locally
        ;; remove compile-time warning
        ;; about wrong type
        (declare #+sbcl
                 (sb-ext:muffle-conditions sb-int:type-warning))

      (test-if-matcher-fails
       "Checked value should be proper alist"
       (has-alist-entries :foo 1 :bar 2)
       not-alist
       "Value is not alist"))))


(deftest plist-assertions
  (let ((value '(:foo 1
                 :bar 2))
        (not-list 1))

    (test-if-matcher-ok
     "Successful match"
     (has-plist-entries :foo 1 :bar 2)
     value
     "Has plist entries:
  :FOO = 1
  :BAR = 2")

    (test-if-matcher-fails
     "Missing value"
     (has-plist-entries :baz 1)
     value
     "Key :BAZ is missing")

    (test-if-matcher-ok
     "Placeholder _ can match any value"
     (has-plist-entries :bar _)
     value
     "Has plist entries:
  :BAR = _")

    (locally
        ;; remove compile-time warning
        ;; about wrong type
        (declare #+sbcl
                 (sb-ext:muffle-conditions sb-int:type-warning))

      (test-if-matcher-fails
       "Checked value should be a list"
       (has-plist-entries :foo 1 :bar 2)
       not-list
       "Value is not a list"))))


(deftest hash-assertions
  (let ((value (make-hash-table :test #'equal))
        (a-number 1)
        (a-list '(1 2 3)))

    (setf (gethash "foo" value) 1
          (gethash "bar" value) 2)

    (test-if-matcher-ok
     "Successful match"
     (has-hash-entries "foo" 1 "bar" 2)
     value
     "Has hash entries:
  \"foo\" = 1
  \"bar\" = 2")

    (test-if-matcher-fails
     "Missing value"
     (has-hash-entries "baz" 1)
     value
     "Key \"baz\" is missing")

    (test-if-matcher-ok
     "Placeholder _ can match any value"
     (has-hash-entries "bar" _)
     value
     "Has hash entries:
  \"bar\" = _")

    (locally
        ;; remove compile-time warning
        ;; about wrong type
        (declare #+sbcl
                 (sb-ext:muffle-conditions sb-int:type-warning))
      
      (test-if-matcher-fails
       "Checked value should be a hash-map"
       (has-hash-entries "foo" 1 "bar"
                         2)
       a-number
       "Value is not a hash")

      (test-if-matcher-fails
       "Checked value should be a hash-map"
       (has-hash-entries "foo" 1 "bar" 2)
       a-list
       "Value is not a hash"))))



(deftest properties-assertions
  (let ((object (make-symbol "Test-Symbol"))
        (a-number 1)
        (a-list '(1 2 3)))

    ;; prepare data for the test, by setting
    ;; these two properties on the symbol
    (setf (get object :foo) 1
          (get object :bar) 2)

    (test-if-matcher-ok
     "Successful match"
     (has-properties :foo 1 :bar 2)
     object
     "Has properties:
  :FOO = 1
  :BAR = 2")

    (test-if-matcher-fails
     "Missing value"
     (has-properties :BAZ 1)
     object
     "Property :BAZ is missing")

    (test-if-matcher-ok
     "Placeholder _ can match any value"
     (has-properties :BAR _)
     object
     "Has properties:
  :BAR = _")

    (locally
        ;; remove compile-time warning
        ;; about wrong type
        (declare #+sbcl
                 (sb-ext:muffle-conditions sb-int:type-warning))
      
      (test-if-matcher-fails
       "Checked value should be a symbol"
       (has-properties :foo 1 :bar 2)
       a-number
       "Value is not a symbol")

      (test-if-matcher-fails
       "Checked value should be a symbol"
       (has-properties :foo 1 :bar 2)
       a-list
       "Value is not a symbol"))))


(defstruct test-class
  (foo)
  (bar))


(deftest slot-assertions
    "Slots assertions"
  (let ((object (make-test-class :foo 1 :bar 2))
        (a-number 1)
        (a-list '(1 2 3)))

    (test-if-matcher-ok
     "Successful match"
     (has-slots 'foo 1 'bar 2)
     object
     "Has slots:
  FOO = 1
  BAR = 2")

    (test-if-matcher-fails
     "Missing value"
     (has-slots 'BAZ 1)
     object
     "Slot BAZ is missing")

    (test-if-matcher-ok
     "Placeholder _ can match any value"
     (has-slots 'BAR _)
     object
     "Has slots:
  BAR = _")

    (locally
        ;; remove compile-time warning
        ;; about wrong type
        (declare #+sbcl
                 (sb-ext:muffle-conditions sb-int:type-warning))
      
      (test-if-matcher-fails
       "Checked value should be an instance"
       (has-slots 'foo 1 'bar 2)
       a-number
       "Value is not an instance")

      (test-if-matcher-fails
       "Checked value should be an instance"
       (has-slots 'foo 1 'bar 2)
       a-list
       "Value is not an instance"))))


(deftest any-matcher-and-placeholder
  (test-if-matcher-ok
   "'Any' matcher matches any value"
   (any)
   1
   "Any value is good enough"))


(deftest contains-matcher
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


(deftest contains-in-anyorder
  (let ((value (list 4 5 3 1))
        (complex (list 3 2 '((:foo "bar")) 1)))
    (test-if-matcher-ok
     "Works with list and don't modifies it"
     (contains-in-any-order 1 4 3 5)
     value
     "Contains all given values")
    ;; now check that original value does not touched
    (ok (equal value
               (list 4 5 3 1)))

    (test-if-matcher-fails
     "And fails if some item not found in given list"
     (contains-in-any-order 1 4 2 5)
     value
     "Value 2 is missing")

    (test-if-matcher-fails
     "And fails if some complex item not found in given list"
     (contains-in-any-order
      1 2 3
      (has-alist-entries :blah "minor"))
     complex
     "(?s)Value which \"Has alist entries:.*\" is missing")))


(deftest test-has-all
    "Grouping matchers with (has-all ...)"

  (let ((value '(:foo "bar" :blah "minor")))
    
    (test-if-matcher-ok
     "Good, if value matches both matchers"
     (has-all (has-plist-entries :foo "bar")
              (has-plist-entries :blah "minor"))
     value
     "All checks are passed")

    (test-if-matcher-fails
     "Matcher 'and' should fail if some matcher fails"
     (has-all (has-plist-entries :foo "bar")
              (has-plist-entries :blah "other"))
     value
     "Key :BLAH has \"minor\" value, but \"other\" was expected")))


(deftest nested-object-matchers
  (let* ((matcher (has-plist-entries
                   :foo (has-alist-entries
                         :bar :minor)))
         (description (matcher-description matcher)))
    
    (ok (equal description
               "Has plist entries:
  :FOO = Has alist entries:
           :BAR = :MINOR"))))


(deftest test-hasnt-plist-keys
  (let ((obj '(:foo "bar")))

    (locally
        ;; remove compile-time warning
        ;; about wrong type
        (declare #+sbcl
                 (sb-ext:muffle-conditions sb-int:type-warning))

      (test-if-matcher-fails
       "It only accepts lists"
       (hasnt-plist-keys :blah)
       42
       "Value is not a list"))
    
    (test-if-matcher-ok
     "If key is absent, than it is good"
     (hasnt-plist-keys :blah)
     obj
     "Key :BLAH is absent")

    (test-if-matcher-ok
     "For multiple keys message should be plural"
     (hasnt-plist-keys :blah :minor)
     obj
     "Keys :BLAH, :MINOR are absent")

    (test-if-matcher-fails
     "If key is present, then matcher should fail"
     (hasnt-plist-keys :foo)
     obj
     "Key :FOO is present in object, but shouldn't")))


(deftest test-list-length-matcher
  (test-if-matcher-ok
   "If list length is equal to specified, it is OK"
   (has-length 4)
   '(a b c d)
   "Has length of 4")

  (test-if-matcher-fails
   "If list length is not equal to specified, it fails"
   (has-length 42)
   '(a b c d)
   "List (A B C D) has length of 4, but 42 was expected")
  
  (test-if-matcher-fails
   "If not a sequence was given, it fails"
   (has-length 42)
   :foo
   "Object :FOO is not of type SEQUENCE"))

