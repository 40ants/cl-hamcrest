=============
 cl-hamcrest
=============

.. image:: https://travis-ci.org/40ants/cl-hamcrest.svg?branch=master
    :target: https://travis-ci.org/40ants/cl-hamcrest

Is implementation of `Hamcrest <http://hamcrest.org>`_ idea in Common Lisp.

It simplifes unittest's code and make it is more readable.

For example, you can write not:

.. code:: lisp

  (let ((value '((:users . (((:firstname . "Maria")
                             (:lastname . "Vasileva")
                             (:age . 21))
                             ((:firstname . "Ivan")
                             (:lastname . "Petrov")
                             (:age . 35))))
                 (:total . 2))))
     (is (cdr (assoc :total value))
         2)
     (let ((sorted-usernames
            (sort (loop for user in (cdr (assoc :users value))
                        collect (assoc :firstname user))
                  #'string<)))
        (is sorted-usernames
            '("Ivan" "Maria")))


You can do just:

.. code:: lisp
  
  (let ((value '((:users . (((:firstname . "Maria")
                             (:lastname . "Vasileva")
                             (:age . 21))
                             ((:firstname . "Ivan")
                             (:lastname . "Petrov")
                             (:age . 35))))
                 (:total . 2))))

     (assert-that value
       (has-alist-entries
         :total 2
         :users (contains-in-any-order
           (has-alist-entries :name "Ivan")
           (has-alist-entries :name "Maria"))))


Supported matchers
==================

* ``contains`` – checks is sequence contains only particular values in correct order.
* ``contains-in-any-order`` – same as ``contains``, but order does not matter.
* ``any`` – matches to any value, have shortcut ``_``.
* ``has-all`` – matches only if all nested matchers match, like ``(and ...)`` logic.
* ``has-alist-entries`` – checks that value is alist, having specified keys and values.
* ``has-plist-entries`` – checks that value is a plist, having specified keys and values.


Roadmap
=======

* Output a context information when some nested matcher was failed.
* Add ``has-hash-entries`` matcher.
* Add ``has-properties`` matcher.
* Add ``has-slots`` matcher.
