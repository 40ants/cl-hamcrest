=============
 cl-hamcrest
=============

.. image:: https://travis-ci.org/40ants/cl-hamcrest.svg?branch=master
    :target: https://travis-ci.org/40ants/cl-hamcrest

Is implementation of `Hamcrest`_ idea in Common Lisp.

It simplifes unittest's code and make it is more readable. Hamcrest uses
idea of patternmatching, to construct matchers from different pieces and
to apply them to the data.

You may ask: "Why dont use a pattern-matching library, like `Optima`_?"

Here is another example from another library ``log4cl-json``, where I want
to check that some fields in plist have special values and other key is not
present. Here is the data:

.. code:: lisp

          (defvar log-line '(:|@message| "Some"
                             :|@timestamp| 122434342
                             ;; this field is wrong and
                             ;; shouldn't be here
                             :|@fields| nil))

With `Optima`_ I could write this code to match the data:

.. code:: lisp

          (ok (ematch
                data
              ((and (guard (property :|@message| m)
                           (equal m "Some"))
                    (property :|@timestamp| _)
                    (not (property :|@fields| _)))
               t))
            "Log entry has message, timestamp, but not fields")

But error message will be quite cumbersome::

  × Aborted due to an error in subtest "Simple match"
    Raised an error Can't match ((:|@fields| NIL :|@timestamp|
                                  "2017-01-03T16:42:00.991444Z" :|@message|
                                  "Some")) with ((COMMON-LISP:AND
                                                  (GUARD
                                                   (PROPERTY :|@message| M)
                                                   (EQUAL M "Some"))
                                                  (PROPERTY :|@timestamp|
                                                   _)
                                                  (NOT
                                                   (PROPERTY :|@fields|
                                                    _)))). (expected: :NON-ERROR)

With ``cl-hamcrest`` test becomes more readable:

.. code::

   (assert-that
         data
         (has-plist-entries :|@message| "Some"
                            :|@timestamp| _)
         (hasnt-plist-keys :|@fields|))

As well, as output about the failure::

  × Key :|@fields| is present in object, but shouldn't

That is because ``cl-hamcrest`` tracks the context and works
together with testing framework, to output all information
to let you understand where the problem is.

To draw a full picture, here is test, written in plain Prove's
assertions:

.. code:: lisp

          (ok (member :|@message| data))
          (is (getf data :|@message|)
              "Some")
          (ok (member :|@timestamp| data))
          (ok (not (member :|@fields| data)))

And it's output::

      ✓ (:|@message| "Some") is expected to be T 
      ✓ "Some" is expected to be "Some" 
      ✓ (:|@timestamp| "2017-01-03T16:57:17.988810Z" :|@message| "Some") is expected to be T 
      × NIL is expected to be T 

is not as clear, if you'll try to figure out
what does ``NIL is expected to be T`` mean.


Supported matchers
==================

* ``contains`` – checks is sequence contains only particular values in correct order.
* ``contains-in-any-order`` – same as ``contains``, but order does not matter.
* ``any`` – matches to any value, have shortcut ``_``.
* ``has-all`` – matches only if all nested matchers match, like ``(and ...)`` logic.
* ``has-alist-entries`` – checks that value is alist, having specified keys and values.
* ``has-plist-entries`` – checks that value is a plist, having specified keys and values.
* ``hasnt-plist-keys`` – checks if give keys are missing in the object.


Roadmap
=======

* Use uniq CommonLisp feature to restart signaled conditions to collect
  all problems with data when there are few problems with keys.
* Add ``has-hash-entries`` matcher.
* Add ``has-properties`` matcher.
* Add ``has-slots`` matcher.


.. _Hamcrest: http://hamcrest.org
.. _Optima: http://quickdocs.org/optima/
