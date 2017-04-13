(in-package :cl-user)
(defpackage hamcrest.matchers
  (:use :cl
        :iterate
        :hamcrest.utils)
  (:import-from :alexandria
                :with-gensyms)
  (:export :has-all
           :has-alist-entries
           :has-plist-entries
           :has-hash-entries
           :has-properties
           :has-slots
           :hasnt-plist-keys
           :any
           :has-length
           :contains
           :contains-in-any-order
           :matcher-description
           :_))

(in-package :hamcrest.matchers)


(defvar *matcher-descriptions*
  (make-hash-table)
  "In some implementation it is impossible to have documentation
in functions created with flet, labels or lambda, that is why
we'll store their docstrings in this cache")


(defun matcher-description (fn)
  "Returns description of a given matcher function.

Can be used to print nested matchers in a nicely indented,
human readable way:

.. code-block:: common-lisp-repl

   TEST> (matcher-description (has-length 100500))
   \"Has length of 100500 \"
   
   TEST> (matcher-description (contains
                               (has-plist-entries :foo \"bar \")
                               (has-plist-entries :foo \"minor \")))
   \"Contains all given values \"
   
   TEST> (matcher-description (has-plist-entries
                               :foo \"bar \"
                               :blah (has-hash-entries :minor \"again \")))
   \"Has plist entries:
     :FOO = \"bar\"
     :BLAH = Has hash entries:
               :MINOR = \"again\"\"
"
  (gethash fn *matcher-descriptions*))


(defsetf matcher-description (fn) (value)
  `(setf (gethash ,fn *matcher-descriptions*)
         ,value))


(defvar *context*
  nil
  "Context description for nested matchers.

When some matcher calls another, it should push it's description to this list.
And after successful matching to pop item from the list.")


(defmacro with-context (description &body body)
  "Manages *context* stack when calling nested matchers."

  `(let ((*context* (cons ,description *context*)))
     ,@body))


(define-condition assertion-error (error)
  ((reason :initarg :reason
           :reader assertion-error-reason)
   ;; save current matcher's context for futher usage
   (context :initform (copy-list *context*)
            :reader assertion-context))
  
  (:report (lambda (condition stream)
             (write-string (assertion-error-reason condition)
                           stream))))


(defmethod print-object ((obj assertion-error) stream)
  "Returns assertion error representation along with it's reason."
  (format stream "#<ASSERTION-ERROR ~a>"
          (assertion-error-reason obj)))


(defun assertion-error-reason-with-context (condition &key (indent-spaces 2))
  "Returns a multiline string where error reason is nested into the context
like that:

Item with index 1:
  Alist entry with key :NAME
    Alist entry with key :FIRST is required

Parameter :indent-spaces could be specified to control number of spaces
for each indentation level."

  (let ((reason (assertion-error-reason condition))
        (level 0))

    (with-output-to-string (s)
      (flet ((indent-line ()
               (iter (repeat (* level indent-spaces))
                     (write-char #\Space s))))

        ;; write context lines
        (iter (for item :in (assertion-context condition))
              (indent-line)
              (write-string item s)
              (write-char #\Newline s)
              (incf level))

        ;; and reason
        (indent-line)
        (write-string reason s)))))


(defun check-if-list (value)
  "A little helper, to check types in matchers"
  (unless (listp value)
    (error 'assertion-error
           :reason "Value is not a list")))


(defun check-if-alist (value)
  "A little helper, to check types in matchers"
  (unless (alistp value)
    (error 'assertion-error
           :reason "Value is not alist")))


(defun check-if-hash (value)
  "A little helper, to check types in matchers"
  (unless (hash-table-p value)

    (error 'assertion-error
           :reason "Value is not a hash")))


(defun check-if-symbol (value)
  "A little helper, to check types in matchers."
  (unless (symbolp value)

    (error 'assertion-error
           :reason "Value is not a symbol")))


(defun check-if-has-slots (object)
  "A little helper, to check if object is instance of a class and has slots."
  (typecase (class-of object)
    ((or standard-class
         structure-class)
     t)
    (t (error 'assertion-error
              :reason "Value is not an instance"))))


(defun quote-underline (value)
  (if (and (symbolp value)
           (equal (symbol-name value)
                  "_"))
      ''_
      value))


(defmacro format-expected-entries (prefix-text)
  "This macro is for formatting description of matchers which expect
  some key/value entries, like (has-plist-entries :foo 1 :bar 2)."

  (let ((message-text (concatenate 'string
                                   prefix-text
                                   ":~%  ~{~a~^~%  ~}")))
    `(format nil ,message-text
             (iterate (for (key value) :on entries :by #'cddr)
                      (collecting
                        (if (functionp value)
                            (let* ((value-prefix (format nil "~S = "
                                                         key))
                                   (indent (prove.reporter::indent
                                            1
                                            (+ (length value-prefix)
                                               ;; add two spaces becase
                                               ;; this is additional
                                               ;; shift for key values
                                               2)))
                                   (description (matcher-description value))
                                   (shifted-value (shift-rest-lines
                                                   description
                                                   indent)))
                              (concatenate 'string
                                           value-prefix
                                           shifted-value))
                            (format nil "~S = ~S"
                                    key
                                    value)))))))


(defmacro def-has-macro (macro-name
                         documentation
                         &key
                           check-obj-type
                           get-key-value
                           format-error-message
                           format-matcher-description)
  "Defines a new macro to check if object has some properties."
  
  `(defmacro ,macro-name (&rest entries)
     ,documentation
     (let ((get-key-value ',get-key-value)
           (check-obj-type ',check-obj-type)
           (format-matcher-description ',format-matcher-description)
           (format-error-message ',format-error-message))
       
       (with-gensyms (expected-key expected-value matcher)
         `(symbol-macrolet ((_ (any)))
            (labels ((format-matcher-description (entries)
                       ,format-matcher-description)
                     (format-error-message (expected-key
                                            expected-value
                                            key-value)
                       ,format-error-message)
                     (,matcher (object)
                       ,check-obj-type
                       
                       ;; we go through each key/value pair
                       ;; from expected entries
                       (flet ((get-key-value (key)
                                "Gets value for the key or throws
condition 'assertion-error with reason \"Key ~S is missing\"."
                                ,get-key-value))
                         
                         (iter (for (,expected-key ,expected-value)
                                    :on (list ,@entries)
                                    :by #'cddr)
                               (let ((key-value (get-key-value ,expected-key)))

                                 ;; if expected-value is callable, then it is a matcher
                                 ;; and we should call it to check
                                 (if (functionp ,expected-value)
                                     (funcall ,expected-value key-value)
                                     ;; or check if it's value is same as specified
                                     (when (not (equal key-value
                                                       ,expected-value))
                                       (error 'assertion-error
                                              :reason (format-error-message
                                                       ,expected-key
                                                       ,expected-value
                                                       key-value)))))))
                       ;; return true to show
                       ;; that mathing was successful
                       t))
              
              (let ((description (format-matcher-description
                                  ;; Here we need a little magic quoting
                                  ;; to make symbols appear as BLAH instead
                                  ;; of 'BLAH or (QUOTE BLAH) (depending on LISP implementaion).
                                  (list ,@(mapcar #'quote-underline entries)))))
                
                (setf (matcher-description (function ,matcher))
                      description)
                (function ,matcher))))))))


(def-has-macro
    has-plist-entries
    "Matches plist entries:

.. code-block:: common-lisp-repl

   TEST> (let ((obj '(:foo :bar)))
           (assert-that obj
                        (has-plist-entries :foo \"bar\"
                                           :blah \"minor\")))
     × Key :FOO has :BAR value, but \"bar\" was expected

This way you can test any number of plist's entries."
    
    :check-obj-type (check-if-list object)
    :get-key-value (let ((key-value (getf object key 'absent)))
                     (when (eql key-value 'absent)
                       (error 'assertion-error
                              :reason (format nil "Key ~S is missing" key)))
                     key-value)
    :format-error-message (format nil "Key ~S has ~S value, but ~S was expected"
                                  expected-key
                                  key-value
                                  expected-value)
    :format-matcher-description (format-expected-entries "Has plist entries"))



(def-has-macro
    has-alist-entries
    "Matches alist entries:

.. code-block:: common-lisp-repl

   TEST> (let ((obj '((:the-key . \"value\"))))
           (assert-that obj
                        (has-alist-entries :the-key \"value\")))
   
     ✓ Has alist entries:
         :THE-KEY = \"value\"
   
   TEST> (let ((obj '((:the-key . \"value\"))))
           (assert-that obj
                        (has-alist-entries :the-key \"value\"
                                           :missing-key \"value\")))
   
     × Key :MISSING-KEY is missing
   
   TEST> (let ((obj '((:the-key . \"value\"))))
           (assert-that obj
                        (has-alist-entries :the-key \"other-value\")))
   
     × Key :THE-KEY has \"value\" value, but \"other-value\" was expected
"
  
    :check-obj-type (check-if-alist object)
    :get-key-value (let* ((pair (assoc key object))
                          (key-value (cdr pair)))
                     (when (null pair)
                       (error 'assertion-error
                              :reason (format nil "Key ~S is missing" key)))
                     key-value)
    :format-error-message (format nil "Key ~S has ~S value, but ~S was expected"
                                  expected-key
                                  key-value
                                  expected-value)
    :format-matcher-description (format-expected-entries "Has alist entries"))


(def-has-macro
    has-hash-entries
    "Matches hash entries:

.. code-block:: common-lisp-repl

   TEST> (let ((obj (make-hash-table)))
           (setf (gethash 'the-key obj) \"value\")
           (assert-that obj
                        (has-hash-entries 'the-key \"value\")))
   
     ✓ Has hash entries:
         THE-KEY = \"value\"

   TEST> (let ((obj (make-hash-table)))
           (setf (gethash 'the-key obj) \"value\")
           (assert-that obj
                        (has-hash-entries 'missing-key \"value\")))
   
     × Key MISSING-KEY is missing

   TEST> (let ((obj (make-hash-table)))
           (setf (gethash 'the-key obj) \"value\")
           (assert-that obj
                        (has-hash-entries 'the-key \"other-value\")))
   
     × Key THE-KEY has \"value\" value, but \"other-value\" was expected
"
  
    :check-obj-type (check-if-hash object)
    :get-key-value (let* ((key-value (gethash key object 'absent)))
                     (when (eql key-value 'absent)
                       (error 'assertion-error
                              :reason (format nil "Key ~S is missing" key)))
                     key-value)
    :format-error-message (format nil "Key ~S has ~S value, but ~S was expected"
                                  expected-key
                                  key-value
                                  expected-value)
    :format-matcher-description (format-expected-entries "Has hash entries"))


(def-has-macro
    has-properties
    "Matches object properties:

.. code-block:: common-lisp-repl

   TEST> (defvar the-object)
   THE-OBJECT
   TEST> (setf (getf the-object :tags) '(one two))
   TEST> (assert-that 'the-object
                      (has-properties :tags '(one two)))
     ✓ Has properties:
         :TAGS = (ONE TWO)
   
   TEST> (assert-that 'the-object
                      (has-properties :tags 'wrong-value))
     × Property :TAGS has (ONE TWO) value, but WRONG-VALUE was expected
   
   TEST> (assert-that 'the-object
                      (has-properties :missing-property '(one two)))
     × Property :MISSING-PROPERTY is missing
"
  
    :check-obj-type (check-if-symbol object)
    :get-key-value (let* ((key-value (get object key 'absent)))
                     (when (eql key-value 'absent)
                       (error 'assertion-error
                              :reason (format nil "Property ~S is missing" key)))
                     key-value)
    :format-error-message (format nil "Property ~S has ~S value, but ~S was expected"
                                  expected-key
                                  key-value
                                  expected-value)
    :format-matcher-description (format-expected-entries "Has properties"))



(def-has-macro
    has-slots
    "Matches object slots:

.. code-block:: common-lisp-repl

   TEST> (defstruct task
           title
           description)
   
   TEST> (defvar task (make-task :title \"The title \"))
   
   TEST> (assert-that task
                      (has-slots 'title \"The title \"))
     ✓ Has slots:
         TITLE = \"The title \"
   
   TEST> (assert-that task
                      (has-slots 'title \"Wrong title \"))
     × Slot TITLE has \"The title \" value, but \"Wrong title \" was expected
   
   TEST> (assert-that task
                      (has-slots 'description nil))
     ✓ Has slots:
         DESCRIPTION = NIL
"
  
    :check-obj-type (check-if-has-slots object)
    :get-key-value (if (and (slot-exists-p object key)
                            ;; slot-bound-p works
                            ;; only for decendants of standard-class
                            ;; for structure-class it always returns
                            ;; true or signals error on some implementations.
                            (typecase (class-of object)
                              (standard-class (slot-boundp object key))
                              (otherwise t)))
                       (slot-value object key)
                       (error 'assertion-error
                              :reason (format nil "Slot ~S is missing" key)))
    :format-error-message (format nil "Slot ~S has ~S value, but ~S was expected"
                                  expected-key
                                  key-value
                                  expected-value)
    :format-matcher-description (format-expected-entries "Has slots"))


(defmacro hasnt-plist-keys (&rest keys)
"Checks if given keys are missing from an object:

.. code-block:: common-lisp-repl

   TEST> (let ((obj '(:foo \"bar\")))
           (assert-that obj
                        (hasnt-plist-keys :blah :minor)))
     ✓ Keys :BLAH, :MINOR are absent

Assertion fails if at least one key is present in the object:

.. code-block:: common-lisp-repl

   TEST> (let ((obj '(:foo \"bar\")))
           (assert-that obj
                        (hasnt-plist-keys :blah :foo)))
     × Key :FOO is present in object, but shouldn't.
"

  (with-gensyms (matcher)
    `(flet ((,matcher (value)
              (check-if-list value)
              
              (iterate (for key :in ',keys)
                       (for key-value next (getf value key 'absent))
                       (when (not (eql key-value 'absent))
                         (error 'assertion-error
                                :reason (format nil "Key ~S is present in object, but shouldn't"
                                                key))))
              ;; if everything is OK, then
              t))

       (let ((description (if (> (length ',keys) 1)
                              (format nil "Keys ~{~S~^, ~} are absent"
                                      ',keys)
                              (format nil "Key ~S is absent" ,@keys))))
         (setf (matcher-description (function ,matcher))
               description)
         (function ,matcher)))))

(defun any ()
  "Assertion is passed regardles of value of the object:

.. code-block:: common-lisp-repl

   TEST> (assert-that 1 (any))
     ✓ Any value if good enough
   
   TEST> (assert-that \"the-string\" (any))
     ✓ Any value if good enough
   
   TEST> (assert-that 'the-symbol (any))
     ✓ Any value if good enough
   
   TEST> (assert-that '(1 2 3) (any))
     ✓ Any value if good enough
"
  (let ((matcher (lambda (value)
                   (declare (ignore value))
                   t))
        (description "Any value is good enough"))
    
    (setf (matcher-description matcher)
          description)
    matcher))


(defun has-all (&rest matchers)
  "Makes a matcher which groups another matchers with AND logic.

This way we can check if plist has one key and hasn't another.
And if all matchers succeed, then ``has-all`` succeed as well:

.. code-block:: common-lisp-repl

   TEST> (assert-that '(:foo \"bar\")
                      (has-all (has-plist-entries :foo \"bar\")
                               (hasnt-plist-keys :blah)))
     ✓ All checks are passed

If at least one check is failed, then ``has-all`` fails too:

.. code-block:: common-lisp-repl

   TEST> (assert-that '(:foo \"bar\" :blah \"minor\")
                      (has-all (has-plist-entries :foo \"bar\")
                               (hasnt-plist-keys :blah)))
     × Key :BLAH is present in object, but shouldn't
"
  (let ((matcher (lambda (value)
                   (iterate (for matcher :in matchers)
                            (funcall matcher value))
                   t))
        (description "All checks are passed"))
    (setf (matcher-description matcher) description)
    matcher))


(defun has-length (expected-length)
  "Checks if a list have specivied length.

.. code-block:: common-lisp-repl

   TEST> (assert-that 'nil (has-length 0))
     ✓ Has length of 0
   
   TEST> (assert-that '(a b c d) (has-length 4))
     ✓ Has length of 4
   
   TEST> (assert-that '(a b c d) (has-length 100500))
     × List (A B C D) has length of 4, but 100500 was expected
"

  (let ((matcher (lambda (value)
                   ;; does nothing yet
                   (let ((obj-length (typecase value
                                       (sequence (length value))
                                       (t (error 'assertion-error
                                                 :reason (format nil "Object ~S is not of type SEQUENCE"
                                                                 value))))))
                     (when (not (eql obj-length
                                     expected-length))
                       (error 'assertion-error
                              :reason (format nil "List ~S has length of ~a, but ~a was expected"
                                              value
                                              obj-length
                                              expected-length))))
                   t))
        (description (format nil "Has length of ~a"
                             expected-length)))
    (setf (matcher-description matcher) description)
    matcher))


(defmacro contains (&rest entries)
  "Checks if each item from a list matches to given matchers.

Contains can accept as raw values, as another matchers:

.. code-block:: common-lisp-repl

   TEST> (assert-that '(:foo
                        (a b c)
                        d)
                      (contains :foo
                                (has-length 3)
                                'd))
     ✓ Contains all given values

Given list should have a length equal to count of matchers:

.. code-block:: common-lisp-repl

   TEST> (assert-that '(:foo
                        (a b c)
                        d)
                      (contains :foo))
     × Expected value is shorter than result

You can ignore value of some list items, by using ``(any)`` matcher:

.. code-block:: common-lisp-repl

   TEST> (assert-that '(:foo
                        (a b c)
                        d)
                      (contains :foo (any) (any)))
     ✓ Contains all given values
"
  (with-gensyms (matcher)
    `(symbol-macrolet ((_ (any)))
       (flet ((,matcher (value)
                (let ((entries-len (length (list ,@entries)))
                      (value-len (length value)))
                  (when (< value-len entries-len)
                    (error 'assertion-error
                           :reason "Result is shorter than expected"))
                  (when (> value-len entries-len)
                    (error 'assertion-error
                           :reason "Expected value is shorter than result"))
                  (iter (for checked-value
                             :in value)
                        (for expected-value
                             :in (list ,@entries))
                        (for index
                             :upfrom 0)

                        (if (functionp expected-value)
                            ;; if expected-value is a matcher
                            (with-context (format nil "Item with index ~a" index)
                              (funcall expected-value checked-value))
                            ;; if it is a real value
                            (unless (equal checked-value
                                           expected-value)
                              (error 'assertion-error
                                     :reason (format nil
                                                     "Item ~S at index ~a, but ~S was expected"
                                                     checked-value
                                                     index
                                                     expected-value))))))
                ;; to show that everything is ok
                t))
         (let ((description "Contains all given values"))
           (setf (matcher-description (function ,matcher))
                 description)
           (function ,matcher))))))


(defmacro contains-in-any-order (&rest entries)
  "Same as ``contains``, but items in the sequence can be in any order:

.. code-block:: common-lisp-repl

   TEST> (assert-that '(:foo
                        (a b c)
                        d)
                      (contains-in-any-order
                       (has-length 3)
                       'd
                       :foo))
   
     ✓ Contains all given values
"
  (with-gensyms (matcher)
    `(flet ((,matcher (value)
              
              (let ((entries-len (length (list ,@entries)))
                    (value-len (length value)))
                (when (< value-len entries-len)
                  (error 'assertion-error
                         :reason "Result is shorter than expected"))
                (when (> value-len entries-len)
                  (error 'assertion-error
                         :reason "Expected value is shorter than result"))
                (iter (for item in (list ,@entries))
                      (unless (find item value
                                    :test (lambda (expected checked-item)
                                            (if (functionp expected)
                                                ;; pass value to next matcher
                                                (handler-case
                                                    (progn
                                                      (funcall expected checked-item)
                                                      ;; if matched, then return True
                                                      t)
                                                  (assertion-error (c)
                                                    (declare (ignorable c))
                                                    ;; if condition was thrown, then item
                                                    ;; does not conform to the matcher
                                                    nil))
                                                ;; otherwise, just check for equality
                                                (equal checked-item expected)
                                                )))
                        (error 'assertion-error
                               :reason (if (functionp item)
                                           (format nil
                                                   "Value which ~S is missing"
                                                   (or (matcher-description item)
                                                       ;; if for some reason matcher's description
                                                       ;; wasn't found
                                                       item))
                                           (format nil
                                                   "Value ~S is missing"
                                                   item))))))
              ;; return true to show that everything is ok
              t))
       
       (let ((description "Contains all given values"))
         (setf (matcher-description (function ,matcher))
               description)
         (function ,matcher)))))
