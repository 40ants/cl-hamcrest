(defpackage hamcrest.matchers
  (:use :cl
        :iterate)
  (:import-from :alexandria
                :with-gensyms)
  (:export :has-alist-entries))

(in-package :hamcrest.matchers)


(define-condition assertion-error (error)
  ((reason :initarg :reason
           :reader assertion-error-reason)))


(defmacro has-alist-entries (&rest entries)
  (with-gensyms (check-key check-value matcher)
    `(flet ((,matcher (value)
              (iter (for (,check-key ,check-value)
                         :on ',entries
                         :by #'cddr)
                    (let ((item (cdr (assoc ,check-key value))))
                      (when (not (equal item
                                        ,check-value))
                        (error 'assertion-error
                               :reason (format nil "Key ~a has ~a value, but ~a was expected"
                                               ,check-key
                                               item
                                               ,check-value)))))))
       (setf (documentation (function ,matcher)
                            'function)
             (format nil "Has alist entries ~s" ',entries))
       (function ,matcher))))
