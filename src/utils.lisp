(defpackage hamcrest.utils
  (:use :cl)
  (:export :alistp))
(in-package :hamcrest.utils)


(defun alistp (value)
  "Checks if give value is a proper alist."
  (when (listp value)
    (every #'consp value)))
