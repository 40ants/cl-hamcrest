(in-package :cl-user)
(defpackage hamcrest.matchers
  (:use :cl
        :iterate
        :hamcrest.utils)
  (:import-from :alexandria
                :with-gensyms)
  (:export :has-alist-entries
           :has-plist-entries
           :hasnt-plist-keys
           :any
           :has-all
           :contains
           :contains-in-any-order
           :_))

(in-package :hamcrest.matchers)


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


(defmacro has-alist-entries (&rest entries)
  (with-gensyms (check-key check-value matcher)
    `(symbol-macrolet ((_ (any)))
       (flet ((,matcher (value)
                (unless (alistp value)
                  (error 'assertion-error
                         :reason "Value is not alist"))
                ;; we go through each key/value pair
                ;; from expected entries
                (iter (for (,check-key ,check-value)
                           :on (list ,@entries)
                           :by #'cddr)
                      (let* ((pair (assoc ,check-key value))
                             (item (cdr pair)))

                        ;; and check if corresponding key is present
                        ;; in original value
                        (when (null pair)
                          (error 'assertion-error
                                 :reason (format nil "Key ~S is missing"
                                                 ,check-key)))

                        ;; and if it is, then pass value to next matcher
                        ;; (which is callable)
                        (if (functionp ,check-value)
                            (funcall ,check-value item)
                            ;; or check if it's value is same as specified
                            (when (not (equal item
                                              ,check-value))
                              (error 'assertion-error
                                     :reason (format nil "Key ~a has ~a value, but ~a was expected"
                                                     ,check-key
                                                     item
                                                     ,check-value))))))
                ;; return true is everything OK
                t))

         (values (function ,matcher)
                 (format nil "Has alist entries ~s" ',entries))))))


(defun check-if-list (value)
  "A little helper, to check types in matchers"
  (unless (listp value)
    (error 'assertion-error
           :reason "Value is not a list")))


(defmacro has-plist-entries (&rest entries)
  "Check if plist have given entries"
  (with-gensyms (expected-key expected-value matcher)
    `(symbol-macrolet ((_ (any)))
       (flet ((,matcher (value)
                (check-if-list value)
                
                ;; we go through each key/value pair
                ;; from expected entries
                (flet ((get-key-value (key)
                         "Get value for the key or throws
condition 'assertion-error with reason \"Key ~S is missing\"."
                         (let ((key-value (getf value key 'absent)))

                           ;; and check if corresponding key is present
                           ;; in original value
                           (when (eql key-value 'absent)
                             (error 'assertion-error
                                    :reason (format nil "Key ~S is missing" key)))
                           key-value)))
                
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
                                       :reason (format nil "Key ~S has ~S value, but ~S was expected"
                                                       ,expected-key
                                                       key-value
                                                       ,expected-value)))))))
                ;; return true to show
                ;; that mathing was successful
                t))
         
         (values (function ,matcher)
                 (format nil "Has plist entries ~S" ',entries))))))


(defmacro hasnt-plist-keys (&rest keys)
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
       
       (values (function ,matcher)
               (if (> (length ',keys) 1)
                   (format nil "Keys ~{~S~^, ~} are absent"
                           ',keys)
                   (format nil "Key ~S is absent" ,@keys))))))

(defun any ()
  (values (lambda (value)
            (declare (ignore value))
            t)
          "Any value if good enough"))


(defun has-all (&rest matchers)
  (values (lambda (value)
            (iterate (for matcher :in matchers)
                     (funcall matcher value))
            t)
          "All checks are passed"))

(defmacro contains (&rest entries)
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
        (values (function ,matcher)
               "Contains all given values")))))


(defun wrap-multiple-values (code)
  "Small helper to use in macro-generation where
item can be a values list"
  `(multiple-value-list ,code))


(defmacro contains-in-any-order (&rest entries)
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
                (iter (for (item item-description) in (list ,@(mapcar #'wrap-multiple-values entries)))
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
                                                   item-description)
                                           (format nil
                                                   "Value ~S is missing"
                                                   item))))))
              ;; return true to show that everything is ok
              t))
       (values (function ,matcher)
               "Contains all given values"))))
