=============
 cl-hamcrest
=============

.. image:: https://travis-ci.org/40ants/cl-hamcrest.svg?branch=master
    :target: https://travis-ci.org/40ants/cl-hamcrest

It is implementation of `Hamcrest`_ idea in Common Lisp.

It simplifes unittests and make them more readable. Hamcrest uses
idea of pattern-matching, to construct matchers from different pieces and
to apply them to the data.

Reasoning
=========

Why not pattern-matching library?
---------------------------------

You may ask: "Why dont use a pattern-matching library, like `Optima`_?"

Here is another example from another library ``log4cl-json``, where I want
to check that some fields in plist have special values and other key is not
present. Here is the data:

.. code:: common-lisp

          (defvar log-item '(:|@message| "Some"
                             :|@timestamp| 122434342
                             ;; this field is wrong and
                             ;; shouldn't be here
                             :|@fields| nil))

With `Optima`_ I could write this code to match the data:

.. code:: common-lisp

          (ok (ematch
                log-item
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

CL-HAMCREST more concise and clear
----------------------------------

With ``cl-hamcrest`` test becomes more readable:

.. code:: common-lisp

   (assert-that
         log-item
         (has-plist-entries :|@message| "Some"
                            :|@timestamp| _)
         (hasnt-plist-keys :|@fields|))

As well, as output about the failure::

  × Key :|@fields| is present in object, but shouldn't

That is because ``cl-hamcrest`` tracks the context and works
together with testing framework, to output all information
to let you understand where the problem is.

Why not just use Prove's assertions?
------------------------------------

To draw a full picture, here is test, written in plain Prove's
assertions:

.. code:: common-lisp

          (ok (member :|@message| log-item))
          (is (getf log-item :|@message|)
              "Some")
          (ok (member :|@timestamp| log-item))
          (ok (not (member :|@fields| log-item)))

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
* ``has-hash-entries`` – checks that value is a hashmap, which have specified keys and values.
* ``has-properties`` – checks if symbol has given properties and values.
* ``has-slots`` – checks if object has give slots and values.
* ``hasnt-plist-keys`` – checks if give keys are missing in the object.


Roadmap
=======

* Use uniq CommonLisp feature to restart signaled conditions to collect
  all problems with data when there are few problems with keys.
* Add ``hasnt-some-keys`` matchers, corresponding to ``has-some-entries``.


.. _Hamcrest: http://hamcrest.org
.. _Optima: http://quickdocs.org/optima/
