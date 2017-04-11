========================
 Integration with Prove
========================

CL-HAMCREST has integration with Prove_.

.. code-block:: common-lisp-repl

   TEST> (ql:quickload :hamcrest-prove)
   TEST> (use-package :hamcrest.prove)
   TEST> (let ((obj (make-hash-table)))
           (assert-that
            obj
            (has-hash-entries :foo :bar)))
     × Key :FOO is missing
   T
   TEST> (let ((obj (make-hash-table)))
           (setf (gethash :foo obj) :bar)

           (assert-that
            obj
            (has-hash-entries :foo :bar)))
     ✓ Has hash entries:
         :FOO = :BAR
   T
   TEST> (let ((obj (make-hash-table)))
           (setf (gethash :foo obj) :bar)

           (assert-that
            obj
            (has-hash-entries :foo :some-value)))
     × Key :FOO has :BAR value, but :SOME-VALUE was expected
   T

This is the simple case, but nested objects can be checked too.

All available matchers are described in chapter :ref:`Matchers library`.

Package `hamcrest.prove`
========================

.. cl:package:: hamcrest.prove

.. cl:macro:: assert-that

.. _Prove: https://github.com/fukamachi/prove

