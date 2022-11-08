(uiop:define-package #:hamcrest-docs/prove
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:hamcrest/prove)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:hamcrest-docs/matchers
                #:@matchers))
(in-package #:hamcrest-docs/prove)

(in-readtable pythonic-string-syntax)


(defsection @prove (:title "Integration with Prove"
                    :external-links (("Prove" . "https://github.com/fukamachi/prove")))
  """
CL-HAMCREST has integration with [Prove][Prove].

```
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
```

This is the simple case, but nested objects can be checked too.

All available matchers are described in the @matchers section.
"""
  (hamcrest/prove:assert-that macro))
