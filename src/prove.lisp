(uiop:define-package #:hamcrest/prove
    (:use #:cl
          #:prove
          #:hamcrest/matchers)
  (:reexport #:hamcrest/matchers)
  (:import-from #:hamcrest/matchers
                #:assertion-error
                #:assertion-error-reason-with-context)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:prove.report)
  (:import-from #:prove.reporter)
  (:import-from #:prove.reporter.list)
  (:import-from #:prove.suite)
  (:export #:assert-that))
(in-package #:hamcrest/prove)


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


(defmethod prove.reporter.list:report-expected-line ((report assertion-report))
  (expected-line report))


(defmethod prove.reporter.list:report-expected-line ((report passed-assertion-report))
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
         (let* ((,matcher-var ,matcher)
                (,matcher-description (matcher-description ,matcher-var)))

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

