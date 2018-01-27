===========
 ChangeLog
===========

0.3.3 (2018-01-28)
==================

* Fixed system's version number.

0.3.2 (2018-01-27)
==================

* Fixed a way how hamcrest/rove reexports symbols from hamcrest/matchers.

0.3.1 (2018-01-26)
==================

* Fixed dependency from cl-ppcre.

0.3.0 (2018-01-26)
==================

* System was refactored to use ``:package-inferred-system`` ASDF option.
  Now all packages use ``/`` instead of dots.
* Now cl-hamcrest supports ``Rove`` test framework, but you need a
  patched version from https://github.com/40ants/rove, because it wasn't
  accepted to the upstream yet.

0.2.1 (2017-10-04)
==================

* Previously, matcher ``contains`` reported "Result is shorter than
  expected" if checked sequence length is lesser than expected, now it
  reports "Result is empty" in case if checked sequence has zero length.

0.2.0 (2017-04-13)
==================

* Added ``length`` matcher.
* Added nice documentation.
* Removed special hack-around to work with Prove (now
  latest Prove's version from repository is required).

0.1.0 (unreleased)
==================

* First version. Contains following matchers:

  - ``has-plist-entries``;
  - ``hasnt-plist-keys``;
  - ``has-alist-entries``;
  - ``has-hash-entries``;
  - ``has-properties``;
  - ``has-slots``;
  - ``contains`` and ``contains-in-any-order``;
  - ``any``;
  - ``has-all``.
