==================
 Matchers library
==================

Here you will find all matchers, supported by CL-HAMCREST, grouped by
their purpose.

Object matchers
===============

This kind of matchers checks some sort of properties on an object. In
this case objects are not only the CLOS objects but also, hashmaps,
alists and property lists.

.. cl:package:: hamcrest.matchers

.. cl:macro:: has-plist-entries

.. cl:macro:: hasnt-plist-keys

.. cl:macro:: has-alist-entries

.. cl:macro:: has-hash-entries

.. cl:macro:: has-properties

.. cl:macro:: has-slots


Sequence matchers
=================

.. cl:function:: has-length

.. cl:macro:: contains

.. cl:macro:: contains-in-any-order


Boolean matchers
================

.. cl:function:: any

.. cl:function:: has-all



Utility functions
=================

.. cl:function:: matcher-description

