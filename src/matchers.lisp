(defpackage hamcrest.matchers
  (:use :cl
        :iterate)
  (:import-from :alexandria
                :with-gensyms)
  (:export :has-alist-entries
           :any
           :_))

(in-package :hamcrest.matchers)


(define-condition assertion-error (error)
  ((reason :initarg :reason
           :reader assertion-error-reason)))


(defmacro has-alist-entries (&rest entries)
  (with-gensyms (check-key check-value matcher)
    `(flet ((,matcher (value)
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
                               :reason (format nil "Key ~a is missing"
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
