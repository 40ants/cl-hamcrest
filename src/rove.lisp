(uiop:define-package hamcrest/rove
  (:use #:cl
        #:hamcrest/matchers)
  (:import-from #:rove/core/result
                #:passed-assertion
                #:*print-assertion*
                #:assertion-values
                #:assertion-reason
                #:stacks
                #:reason
                #:form)
  ;; I had to rewrite some Rove internals
  ;; after the https://github.com/fukamachi/rove/commit/13176ebf2a7ae0f534c66fc9b9dbf1acfa0797ca
  (:import-from #:rove/core/assertion)
  (:import-from #:alexandria
                #:with-gensyms)
  ;; reexport matchers for convenience
  (:reexport #:hamcrest/matchers)
  
  (:export :assert-that))
(in-package :hamcrest/rove)


(defvar *current-matcher-description*)
(defvar *current-matcher-form*)
(defvar *current-object-form*)
(defvar *current-object-value*)


(defclass failed-assertion (rove/core/result:failed-assertion)
  ((error-description :initarg :error-description
                      :initform nil
                      :reader get-error-description)
   (matcher-description :initform *current-matcher-description*
                        :reader get-matcher-description)
   (matcher-form :initform *current-matcher-form*
                 :reader get-matcher-form)
   (object-form :initform *current-object-form*
                :reader get-object-form)
   (object-value :initform *current-object-value*
                 :reader get-object-value)))


(defmethod print-object ((assertion failed-assertion) stream)
  (cond (*print-assertion*
         (format stream "Matcher:")
         (pprint (get-matcher-form assertion) stream)
         (format stream "~2&Object:")
         (pprint (get-object-form assertion) stream)
         (format stream " -> ")
         (pprint (get-object-value assertion) stream))
        (t
         (call-next-method))))


;; These functions were borrowed from Rove and modified to pass our assertion-error class

(defun ok-assertion-class (result error)
  (declare (ignore error))
  (if result
      'passed-assertion
      'failed-assertion))

(defun ng-assertion-class (result error)
  (cond
    (error 'failed-assertion)
    (result 'failed-assertion)
    (t 'passed-assertion)))

(defun %okng-record (form result args-symbols args-values steps stacks reason duration desc positive)
  (let* ((class-fn (if positive
                       #'ok-assertion-class
                       #'ng-assertion-class))
         (assertion
           (make-instance (funcall class-fn
                                   (if (eq result rove/core/assertion::*fail*)
                                       (not positive)
                                       (not (null result)))
                                   reason)
                          :form form
                          :steps steps
                          :args args-symbols
                          :values args-values
                          :reason reason
                          :desc desc
                          :duration duration
                          :stacks stacks
                          :labels (and rove/core/stats::*stats*
                                       (rove/core/stats::stats-context-labels rove/core/stats::*stats*))
                          :negative (not positive))))
    (rove/core/stats::record rove/core/stats::*stats* assertion)
    result))

(defun record-error (form steps reason duration description positive)
  (%okng-record form rove/core/assertion::*fail* nil nil steps (dissect:stack) reason duration description positive))

(defmacro %okng (form desc positive &environment env)
  (declare (ignore env))
  (let* ((form-steps (rove/core/assertion::form-steps form))
         (form (gensym "FORM"))
         (expanded-form (first form-steps))
         (result (gensym "RESULT"))
         (args-symbols (gensym "ARGS-SYMBOLS"))
         (args-values (gensym "ARGS-VALUES"))
         (steps (gensym "STEPS"))
         (e (gensym "E"))
         (start (gensym "START"))
         (block-label (gensym "BLOCK")))
    `(let* ((,start (get-internal-real-time))
            (,form ',expanded-form)
            (,steps ',(reverse form-steps)))
       (block ,block-label
         (handler-bind
             ((error (lambda (,e)
                       (record-error ,form ,steps ,e (rove/core/assertion::calc-duration ,start) ,desc ,positive)
                       (unless (rove/core/assertion::debug-on-error-p)
                         (return-from ,block-label rove/core/assertion::*fail*)))))
           (multiple-value-bind (,result ,args-symbols ,args-values)
               (rove/core/assertion::form-inspect ,expanded-form)
             (%okng-record ,form
                           ,result ,args-symbols ,args-values
                           ,steps
                           nil
                           nil
                           (rove/core/assertion::calc-duration ,start)
                           ,desc
                           ,positive)))))))


(defmacro assert-that (value &rest matchers)
  "Main macro to test values agains matchers."
  
  (let ((matcher (if (> (length matchers)
                        1)
                     ;; if there is more than one matcher,
                     ;; then we need to combine them implicitly
                     `(has-all ,@matchers)
                     ;; otherwise, just use single matcher
                     (first matchers))))
    
    (with-gensyms (matcher-var)
      `(symbol-macrolet ((_ (any)))
         (let* ((,matcher-var ,matcher)
                (*current-matcher-description* (matcher-description ,matcher-var))
                (*current-matcher-form* (matcher-form ,matcher-var))
                (*current-object-form* ',value)
                (*current-object-value* ,value))

           (%okng
            (funcall ,matcher-var ,value)
            *current-matcher-description*
            ;; positive? sure!
            t))))))


(defmethod initialize-instance :after ((assertion failed-assertion) &rest init-args)
  (declare (ignorable init-args))
  (let ((reason (assertion-reason assertion)))
    (typecase reason
      (assertion-error
       (let ((description (assertion-error-reason-with-context reason)))
         (setf
          (slot-value assertion 'stacks)
          nil
          (slot-value assertion 'reason)
          nil
          (slot-value assertion 'error-description)
          description
          (slot-value assertion 'form)
          '(foo bar))
         description)))))


(defmethod rove/core/result:assertion-description ((assertion failed-assertion))
  "Handle assertion error if it was raised during evaluating of some matcher."
  (or (get-error-description assertion)
      (call-next-method)))
