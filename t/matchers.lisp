(uiop:define-package #:hamcrest-tests/matchers
  (:use #:cl
        #:rove
        #:hamcrest/matchers)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:cl-ppcre)
  (:import-from #:hamcrest/matchers
                #:assertion-error
                #:assertion-error-reason
                #:assertion-error-reason-with-context))
(in-package #:hamcrest-tests/matchers)


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


(defclass test-person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age)
   (address :initarg :address :accessor person-address :initform nil)))


(defclass test-address ()
  ((city :initarg :city :accessor address-city)
   (zip :initarg :zip :accessor address-zip)))


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


(deftest test-type-matcher
  (test-if-matcher-ok
   "If object is of given type, it is OK"
   (has-type 'cons)
   '(a b c d)
   "Has type CONS")

  (test-if-matcher-fails
   "If type mismatch, it fails"
   (has-type 'integer)
   '(a b c d)
   "(A B C D) has type CONS, but INTEGER was expected"))


;;; Tests for nested context pushing

(deftest nested-context-assertions
    "Nested matchers should include context in error messages"

  ;; Test that nested has-slots failure includes slot context
  (let ((person (make-instance 'test-person
                               :name "Alice"
                               :age 30
                               :address (make-instance 'test-address
                                                       :city "Boston"
                                                       :zip "02101"))))

    (test-if-matcher-fails
     "Nested has-slots failure includes slot context"
     (has-slots 'address (has-slots 'city "NYC"))
     person
     "Slot ADDRESS
  Slot CITY has \"Boston\" value, but \"NYC\" was expected")

    ;; Test that multi-level nesting shows full context chain
    (test-if-matcher-fails
     "Multi-level nesting shows full context chain"
     (has-slots 'address (has-slots 'zip "99999"))
     person
     "Slot ADDRESS
  Slot ZIP has \"02101\" value, but \"99999\" was expected"))

  ;; Test nested plist context
  (let ((value '(:user (:name "Alice" :age 30))))
    (test-if-matcher-fails
     "Nested has-plist-entries failure includes plist context"
     (has-plist-entries :user (has-plist-entries :name "Bob"))
     value
     "Plist entry :USER
  Key :NAME has \"Alice\" value, but \"Bob\" was expected"))

  ;; Test nested alist context
  (let ((value '((:user . ((:name . "Alice"))))))
    (test-if-matcher-fails
     "Nested has-alist-entries failure includes alist context"
     (has-alist-entries :user (has-alist-entries :name "Bob"))
     value
     "Alist entry :USER
  Key :NAME has \"Alice\" value, but \"Bob\" was expected"))

  ;; Test that non-nested matchers do NOT add context
  (let ((object (make-test-class :foo 1 :bar 2)))
    (test-if-matcher-fails
     "Non-nested slot failure has no context"
     (has-slots 'foo 42)
     object
     "Slot FOO has 1 value, but 42 was expected")))


;;; Tests for instance-of matcher

(deftest instance-of-assertions
    "instance-of matcher checks type and optionally applies nested matchers"

  (let ((person (make-instance 'test-person
                               :name "Alice"
                               :age 30
                               :address (make-instance 'test-address
                                                       :city "Boston"
                                                       :zip "02101"))))

    ;; Type-only check
    (test-if-matcher-ok
     "Correct type with no matchers"
     (instance-of 'test-person)
     person
     "Instance of TEST-PERSON")

    ;; Type + slot matcher
    (test-if-matcher-ok
     "Correct type with slot matcher"
     (instance-of 'test-person
                  (has-slots 'name "Alice" 'age 30))
     person
     "Instance of TEST-PERSON, Has slots:
  NAME = \"Alice\"
  AGE = 30")

    ;; Wrong type
    (test-if-matcher-fails
     "Wrong type fails"
     (instance-of 'test-address)
     person
     "(?s).*has type TEST-PERSON, but TEST-ADDRESS was expected")

    ;; Right type, nested matcher fails
    (test-if-matcher-fails
     "Right type but nested matcher fails"
     (instance-of 'test-person
                  (has-slots 'name "Bob"))
     person
     "Instance of TEST-PERSON
  Slot NAME has \"Alice\" value, but \"Bob\" was expected")

    ;; Combined instance-of with nested has-slots for deep structure
    (test-if-matcher-ok
     "Deep nested structure with instance-of"
     (instance-of 'test-person
                  (has-slots 'address
                             (instance-of 'test-address
                                          (has-slots 'city "Boston"))))
     person
     "Instance of TEST-PERSON, Has slots:
  ADDRESS = Instance of TEST-ADDRESS, Has slots:
              CITY = \"Boston\"")

    ;; Deep nested failure with full context
    ;; Note: context is LIFO (innermost first), so Instance of TEST-ADDRESS
    ;; appears before Slot ADDRESS and Instance of TEST-PERSON
    (test-if-matcher-fails
     "Deep nested failure with instance-of context"
     (instance-of 'test-person
                  (has-slots 'address
                             (instance-of 'test-address
                                          (has-slots 'city "NYC"))))
     person
     "Instance of TEST-ADDRESS
  Slot ADDRESS
    Instance of TEST-PERSON
      Slot CITY has \"Boston\" value, but \"NYC\" was expected")))


;;; Tests for has-accessors matcher

(deftest accessor-assertions
    "has-accessors matcher checks object via accessor functions"

  (let ((person (make-instance 'test-person
                               :name "Alice"
                               :age 30
                               :address (make-instance 'test-address
                                                       :city "Boston"
                                                       :zip "02101")))
        (a-number 1)
        (a-list '(1 2 3)))

    ;; Successful match
    (test-if-matcher-ok
     "Successful accessor match"
     (has-accessors 'person-name "Alice" 'person-age 30)
     person
     "Has accessors:
  PERSON-NAME = \"Alice\"
  PERSON-AGE = 30")

    ;; Wrong value
    (test-if-matcher-fails
     "Accessor returns wrong value"
     (has-accessors 'person-name "Bob")
     person
     "Accessor PERSON-NAME returned \"Alice\", but \"Bob\" was expected")

    ;; Placeholder _ can match any value
    (test-if-matcher-ok
     "Placeholder _ can match any value"
     (has-accessors 'person-name _)
     person
     "Has accessors:
  PERSON-NAME = _")

    ;; Nested matchers via accessor
    (test-if-matcher-ok
     "Nested matcher via accessor"
     (has-accessors 'person-address
                    (has-accessors 'address-city "Boston"))
     person
     "Has accessors:
  PERSON-ADDRESS = Has accessors:
                     ADDRESS-CITY = \"Boston\"")

    ;; Nested accessor failure with context
    (test-if-matcher-fails
     "Nested accessor failure includes context"
     (has-accessors 'person-address
                    (has-accessors 'address-city "NYC"))
     person
     "Accessor PERSON-ADDRESS
  Accessor ADDRESS-CITY returned \"Boston\", but \"NYC\" was expected")

    (locally
        ;; remove compile-time warning
        ;; about wrong type
        (declare #+sbcl
                 (sb-ext:muffle-conditions sb-int:type-warning))

      ;; Non-instance input
      (test-if-matcher-fails
       "Non-instance input fails"
       (has-accessors 'person-name "Alice")
       a-number
       "Value is not an instance")

      (test-if-matcher-fails
       "Non-instance list input fails"
       (has-accessors 'person-name "Alice")
       a-list
       "Value is not an instance"))

    ;; Combined with instance-of for the full pattern
    (test-if-matcher-ok
     "Combined instance-of with has-accessors"
     (instance-of 'test-person
                  (has-accessors 'person-name "Alice"
                                 'person-age 30))
     person
     "Instance of TEST-PERSON, Has accessors:
  PERSON-NAME = \"Alice\"
  PERSON-AGE = 30")))

