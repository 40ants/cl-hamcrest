.. cl-hamcrest documentation master file, created by
   sphinx-quickstart on Sat Apr  8 20:18:20 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.


Welcome to cl-hamcrest's documentation!
=======================================

This is an implementation of Hamcrest for Common Lisp.

Here are some examples
----------------------


.. code-block:: common-lisp-repl

   GIT> (assert-that
          log-item
          (has-plist-entries :|@message| "Some"
                             :|@timestamp| _)
          (hasnt-plist-keys :|@fields|))

.. include:: ../../README.rst
   :start-after: include-from
   :end-before: include-to

Contents
========

.. toctree::
   :maxdepth: 2

   matchers
   prove
   changelog


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`


