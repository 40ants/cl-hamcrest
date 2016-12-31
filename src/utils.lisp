(defpackage hamcrest.utils
  (:use :cl
        :iterate)
  (:export :alistp
           :deindent))
(in-package :hamcrest.utils)


(defun alistp (value)
  "Checks if give value is a proper alist."
  (when (listp value)
    (every #'consp value)))


(defun left-remove-if (items predicate)
  "Returns list skipping leftmost items
 which match a predicate."
  (do ()
      ((not (funcall predicate (car items))) items)
    (setf items (cdr items))))


(defun right-remove-if (items predicate)
  "Returns a new list, without rightmost items
which match a predicate."
  (labels ((recur (items)
             (destructuring-bind (head . tail) items
               (if tail
                   (let ((tail (recur tail)))
                     (if tail
                         (cons head tail)
                         (unless (funcall predicate head)
                           (list head))))
                   (if (funcall predicate head)
                       nil
                       (list head))))))
    (recur items)))


(defun get-indentation (line)
  "Returns numbers of leading spaces for the line."
  (iter (for char in-string line)
        (if (not (equal char #\Space))
            (leave num-spaces))
        (counting t into num-spaces)))


(defun empty-line-p (line)
  "Checks if line of text is empty."
  (equal line ""))


(defun deindent (text)
  "Removes empty new lines at the begining and at the end of the text,
and removes common number of whitespaces from rest of the lines."
  ;; remove empty lines at beginning
  (let* ((all-lines (split-sequence:split-sequence
                     #\Newline
                     text))
         (left-trimmed (left-remove-if
                        all-lines
                        #'empty-line-p))
         (lines (right-remove-if
                 left-trimmed
                 #'empty-line-p))
         
         ;; calculate common indentation
         (min-indent (apply #'min (mapcar #'get-indentation lines)))
         
         ;; remove common indentation from lines
         (new-lines (iter (for line in lines)
                          (collect (subseq line min-indent)))))
    
    ;; now join lines together and separate them with new-lines
    (values (format nil "~{~a~^~%~}" new-lines)
            min-indent)))
