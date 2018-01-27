(uiop:define-package hamcrest/rove
  (:use #:cl
        #:prove
        #:iterate
;;        #:hamcrest/matchers
        )
  (:import-from #:rove/core/result
                #:passed-assertion
                #:*print-assertion*
                #:assertion-values
                #:assertion-reason
                #:stacks
                #:reason
                #:form)
  (:import-from #:rove/core/assertion
                #:%okng)
  (:import-from #:hamcrest/matchers
                #:assertion-error
                #:assertion-error-reason-with-context
                #:has-plist-entries)
  (:import-from #:alexandria
                #:with-gensyms)
  (:reexport #:hamcrest/matchers)
  (:export :assert-that))
(in-package :hamcrest/rove)

;; reexport matchers for convenience
;;(cl-reexport:reexport-from 'hamcrest/matchers)


(defvar *current-matcher-description*)
(defvar *current-matcher-form*)


(defclass failed-assertion (rove/core/result:failed-assertion)
  ((error-description :initarg :error-description
                      :initform nil
                      :reader get-error-description)
   (matcher-description :initform *current-matcher-description*
                        :reader get-matcher-description)
   (matcher-form :initform *current-matcher-form*
                 :reader get-matcher-form)))


(defmethod print-object ((assertion failed-assertion) stream)
  ;;  (format stream "Some failed assertion")
  (if *print-assertion*
      (let* ((rove-form-args (assertion-values assertion))
             ;; We call Rove's ok macro with like that:
             ;; (ok (funcall matcher-func object-to-match) ...)
             ;; that is why here we take object-to-match as the
             ;; second argument of the failed form.
             (matcher-arg (second rove-form-args)))
        (format stream "Matcher:")
        (pprint (get-matcher-form assertion) stream)
        (format stream "~2&Object:")
        (pprint matcher-arg stream))
      (call-next-method))
  )


;; (defmacro assert-that-one (value &rest matchers)
;;   "Main macro to test values agains matchers."
  
;;   (let ((matcher (if (> (length matchers)
;;                         1)
;;                      ;; if there is more than one matcher,
;;                      ;; then we need to combine them implicitly
;;                      `(has-all ,@matchers)
;;                      ;; otherwise, just use single matcher
;;                      (first matchers))))
    
;;     (with-gensyms (matcher-var matcher-description)
;;       `(symbol-macrolet ((_ (any)))
;;          (let* ((,matcher-var ,matcher)
;;                 (,*current-matcher-description* (matcher-description ,matcher-var)))

;;            (rove/core/assertion::%okng
;;             (funcall ,matcher-var ,value)
;;             ,*current-matcher-description*
;;             (lambda (result error)
;;               (declare (ignorable error))
;;               (if result
;;                   'rove/core/result:passed-assertion
;;                   'failed-assertion))))))))


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
                (*current-matcher-form* (matcher-form ,matcher-var)))

           (%okng
            (funcall ,matcher-var ,value)
            *current-matcher-description*
            (lambda (result error)
              (declare (ignorable error))
              (if result
                  'passed-assertion
                  'failed-assertion))))))))


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
