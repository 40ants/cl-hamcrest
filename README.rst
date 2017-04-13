=============
 cl-hamcrest
=============

.. image:: https://travis-ci.org/40ants/cl-hamcrest.svg?branch=master
    :target: https://travis-ci.org/40ants/cl-hamcrest

.. include-from

It is implementation of `Hamcrest`_ idea in Common Lisp.

It simplifes unittests and make them more readable. Hamcrest uses
idea of pattern-matching, to construct matchers from different pieces and
to apply them to the data.

Why not pattern-matching library?
=================================

You may ask: "Why dont use a pattern-matching library, like `Optima`_?"

Here is another example from another library ``log4cl-json``, where I want
to check that some fields in plist have special values and other key is not
present. Here is the data:

.. code-block:: common-lisp

   (defvar log-item '(:|@message| "Some"
                      :|@timestamp| 122434342
                      ;; this field is wrong and
                      ;; shouldn't be here
                      :|@fields| nil))

With `Optima`_ I could write this code to match the data:

.. code-block:: common-lisp

   (ok (ematch
         log-item
       ((and (guard (property :|@message| m)
                    (equal m "Some"))
             (property :|@timestamp| _)
             (not (property :|@fields| _)))
        t))
     "Log entry has message, timestamp, but not fields")

But error message will be quite cumbersome:

.. code-block:: none

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


CL-HAMCREST is more concise and clear
-------------------------------------

With ``cl-hamcrest`` test becomes more readable:

.. code-block:: common-lisp

   (assert-that
         log-item
         (has-plist-entries :|@message| "Some"
                            :|@timestamp| _)
         (hasnt-plist-keys :|@fields|))

As well, as output about the failure:

.. code-block:: none

  × Key :|@fields| is present in object, but shouldn't

That is because ``cl-hamcrest`` tracks the context and works
together with testing framework, to output all information
to let you understand where the problem is.

Why not just use Prove's assertions?
------------------------------------

To draw a full picture, here is test, written in plain Prove's
assertions:

.. code-block:: common-lisp

   (ok (member :|@message| log-item))
   (is (getf log-item :|@message|)
       "Some")
   (ok (member :|@timestamp| log-item))
   (ok (not (member :|@fields| log-item)))

And it's output:

.. code-block:: none

   ✓ (:|@message| "Some") is expected to be T 
   ✓ "Some" is expected to be "Some" 
   ✓ (:|@timestamp| "2017-01-03T16:57:17.988810Z" :|@message| "Some") is expected to be T 
   × NIL is expected to be T 

is not as clear, if you'll try to figure out
what does ``NIL is expected to be T`` mean.

Description of all supported matchers, you can `find in the
documentation <http://cl-hamcrest.40ants.com>`_.

Roadmap
=======

* Logical matchers:

  - ``all-of`` – rename ``has-all``.
  - ``any-of`` – Matches if any of the given matchers evaluate to True.
  - ``is-not`` – Inverts the given matcher to its logical negation (think if
    we need it, and how to show the results, here are results
    how it works `in PyHamcrest <https://gist.github.com/svetlyak40wt/fbe480384e9e3f75b10523aa0b4fb6ce>`_
    – it just sees that matcher returned True and raises Assertion error with full object's content and
    matcher's description with prepended 'not' particle).

* Object matchers:

  - Add ``hasnt-some-keys`` matchers, corresponding to
    ``has-some-entries``.
  - Make ``has-alist-entries`` work with keys other than keyword
    right now it uses `eql` to compare keys.

* Sequence matchers:

  - ``is-in`` – Matches if evaluated object is present in a given sequence.
  - ``has-items`` – Matches if all of the given matchers are satisfied by any elements of the sequence.
  - ``only-contains`` – Matches if each element of sequence satisfies
    any of the given matchers.

* Other features:

  - Use uniq CommonLisp feature to restart signaled conditions to collect
    all problems with data when there are few problems with keys.

.. _Hamcrest: http://hamcrest.org
.. _Optima: http://quickdocs.org/optima/

.. include-to

Building Documentation
======================

Requirements
------------

Python packages
~~~~~~~~~~~~~~~

sphinx
sphinxcontrib-cldomain (https://github.com/russell/sphinxcontrib-cldomain)
pygments-cl-repl
sphinx-bootstrap-theme

Lisp
~~~~

cl-launch (http://www.cliki.net/CL-Launch)

To build
--------

cd docs && make html
