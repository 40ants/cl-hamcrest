(defpackage hamcrest.matchers
  (:use :cl
        :iterate
        :hamcrest.utils)
  (:import-from :alexandria
                :with-gensyms)
  (:export :has-alist-entries
           :any
           :contains
           :contains-in-any-order
           :_))

(in-package :hamcrest.matchers)


(define-condition assertion-error (error)
  ((reason :initarg :reason
           :reader assertion-error-reason)))


(defmacro has-alist-entries (&rest entries)
  (with-gensyms (check-key check-value matcher)
    `(flet ((,matcher (value)
              (unless (alistp value)
                (error 'assertion-error
                       :reason "Value is not alist"))
              ;; we go through each key/value pair
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
                                                   ,check-value))))))))
       (setf (documentation (function ,matcher)
                            'function)
             (format nil "Has alist entries ~s" ',entries))
       (function ,matcher))))


(defun any ()
  (lambda (value)
    "Any value if good enough"
    (declare (ignore value))
    nil))


(defmacro contains (&rest entries)
  (with-gensyms (matcher)
    `(flet ((,matcher (value)
              "Contains all given values"
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
                          (funcall expected-value checked-value)
                          ;; if it is a real value
                          (unless (equal checked-value
                                         expected-value)
                            (error 'assertion-error
                                   :reason (format nil
                                                   "Item ~S at index ~a, but ~S was expected"
                                                   checked-value
                                                   index
                                                   expected-value))))))))
       (function ,matcher))))


(defmacro contains-in-any-order (&rest entries)
  (with-gensyms (matcher)
    `(flet ((,matcher (value)
              "Contains all given values"
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
                                                   (documentation item 'function))
                                           (format nil
                                                   "Value ~S is missing"
                                                   item))))))))
       (function ,matcher))))
