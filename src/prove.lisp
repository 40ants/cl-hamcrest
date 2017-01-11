(in-package :cl-user)
(defpackage hamcrest.prove
  (:use :cl
        :prove
        :iterate
        :hamcrest.matchers)
  (:import-from :hamcrest.matchers
                :assertion-error
                :assertion-error-reason-with-context)
  (:import-from :alexandria
                :with-gensyms)
  (:export :assert-that))
(in-package :hamcrest.prove)

;; reexport matchers for convenience
(cl-reexport:reexport-from 'hamcrest.matchers)


(defvar original-report-expected-line
  nil
  "Holder for report-expected-line function, taken from prove.reporter.list.")


(defvar rebind-report-expected-line
  nil
  "A flag. If it is true, then original report-expected-line
should be replaced with new \"generic\" version.")

(when (not (fboundp 'original-report-expected-line))
  (setf (fdefinition 'original-report-expected-line)
        #'prove.reporter.list::report-expected-line)
  (setf rebind-report-expected-line t))


(defgeneric report-expected-line (report)
  (:documentation "Returns a line which describes a problem with failed test."))

(defmethod report-expected-line (report)
  "Default behaviour is to call original Prove's function,
which formats a string, comparing two values."
  (original-report-expected-line report))

(when rebind-report-expected-line
  (setf (fdefinition 'prove.reporter.list::report-expected-line)
        #'report-expected-line)
  (setf rebind-report-expected-line nil))


(defclass assertion-report (prove.report:failed-test-report)
  ((prove.report:got :initform nil)
   (prove.report:expected :initform nil)
   (expected-line :initarg :expected-line
                  :initform (error ":expected-line is required")
                  :reader expected-line)))


(defclass passed-assertion-report (prove.report:normal-test-report)
  ((prove.report:got :initform nil)
   (prove.report:expected :initform nil)
   (expected-line :initarg :expected-line
                  :initform (error ":expected-line is required")
                  :reader expected-line)))

(defmethod report-expected-line ((report assertion-report))
  (expected-line report))

(defmethod report-expected-line ((report passed-assertion-report))
  (expected-line report))

(defmacro assert-that (value &rest matchers)
  "Main macro to test values agains matchers."
  
  (let ((matcher (if (> (length matchers)
                        1)
                     ;; if there is more than one matcher,
                     ;; then we need to combine them implicitly
                     `(has-all ,@matchers)
                     ;; otherwise, just use single matcher
                     (first matchers))))
    
    (with-gensyms (matcher-var matcher-description)
      `(symbol-macrolet ((_ (any)))
         (multiple-value-bind (,matcher-var ,matcher-description)
             ,matcher
       
           (let* ((suite (prove.suite:current-suite))
                  (report (handler-case
                              (progn (funcall ,matcher-var ,value)
                                     (make-instance 'passed-assertion-report
                                                    :expected-line ,matcher-description))
                            (assertion-error (c)
                              (incf (prove.suite:failed suite))
                              (make-instance 'assertion-report
                                             :expected-line (assertion-error-reason-with-context c))))))
             (prove.suite:add-report report suite)
             (incf (prove.suite:test-count suite))
     
             (prove.reporter:format-report *test-result-output* nil report
                                           :count (prove.suite:test-count suite))

             (values t report)))))))

