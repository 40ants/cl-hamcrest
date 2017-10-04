===========
 ChangeLog
===========

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
