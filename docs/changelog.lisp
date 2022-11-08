(uiop:define-package #:hamcrest-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:hamcrest-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "SBCL"
                              "CCL"
                              "HTTP")
               :external-docs ("https://40ants.com/doc"))
  (0.5.0 2022-11-08
         "* Refactored documentation to use 40ANTS-DOC system and moved from CircleCI to GitHub Actions.")
  (0.4.5 2022-02-11
         "* Fixed `assert-that` for working with latest Rove.")
  (0.4.4 2019-06-07
         "* Fixed an issue how `assert-that` reports about failure under the Rove.
  Previously, it always reported that \"Object\" is `nil`, but now it is fixed,
  and output will look like that::

    Expected value is shorter than result
      Matcher:
      (HAMCREST/MATCHERS:CONTAINS 1 2)
 
      Object:
      (LIST 1 2 3) -> 
      (1 2 3)
")
  (0.4.3 2018-12-08
         "* Fixed compatibility with the latest `rove`.
  Bug was caused by [this commit](https://github.com/fukamachi/rove/commit/1f84d70a0b4db03bd0ec9b837fbc961189462f0d).
* Also, now `hamcrest/rove` is covered by tests.
")
  (0.4.2 2018-06-03
         "* Fixed hamcrest/prove compilation under SBCL.
* Travis config now contains only SBCL and CCL.
")
  (0.4.1 2018-06-03
         "* Tests where rewritten to use Rove.")
  (0.4.0 2018-06-02
         "* System `hamcrest-prove` was replaced with `hamcrest/prove` and
  now uses `package-inferred` asdf class.
")
  (0.3.4 2018-01-29
         "* These symbols were exported to fix Rove integration:
  
  * `assertion-error`
  * `assertion-error-reason`
  * `assertion-context`
  * `assertion-error-reason-with-context`

* `contains` matcher now saves it's form to make
  error report better under the Rove.
")
  (0.3.3 2018-01-28
         "* Fixed system's version number.")
  (0.3.2 2018-01-27
         "* Fixed a way how hamcrest/rove reexports symbols from hamcrest/matchers.")
  (0.3.1 2018-01-26 "* Fixed dependency from cl-ppcre.")
  (0.3.0 2018-01-26
         "* System was refactored to use `:package-inferred-system` ASDF option.
  Now all packages use `/` instead of dots.
* Now cl-hamcrest supports `Rove` test framework, but you need a
  patched version from https://github.com/40ants/rove, because it wasn't
  accepted to the upstream yet.
")
  (0.2.1 2017-10-04
         "* Previously, matcher `contains` reported \"Result is shorter than
  expected\" if checked sequence length is lesser than expected, now it
  reports \"Result is empty\" in case if checked sequence has zero length.
")
  (0.2.0 2017-04-13
         "* Added `length` matcher.
* Added nice documentation.
* Removed special hack-around to work with Prove (now
  latest Prove's version from repository is required).
")
  (0.1.0 2017-04-02
         "* First version. Contains following matchers:

  - `has-plist-entries`;
  - `hasnt-plist-keys`;
  - `has-alist-entries`;
  - `has-hash-entries`;
  - `has-properties`;
  - `has-slots`;
  - `contains` and `contains-in-any-order`;
  - `any`;
  - `has-all`.
"))
