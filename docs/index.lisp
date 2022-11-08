(uiop:define-package #:hamcrest-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:quicklisp)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:hamcrest-docs/changelog
                #:@changelog)
  (:import-from #:hamcrest-docs/matchers
                #:@matchers)
  (:import-from #:hamcrest-docs/prove
                #:@prove)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:hamcrest-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "hamcrest-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  (ql:quickload "40ants-doc-theme-40ants")
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "Implementation of Hamcrest for Common Lisp"
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "HAMCREST"
                                   "CL-HAMCREST"
                                   "CLOS"
                                   "REPL"
                                   "GIT"
                                   "BSD"
                                   "SBCL"
                                   "CCL"
                                   "LOG4CL"
                                   "THIS-CONSOLE"
                                   "DAILY"
                                   "FILE"))
  (hamcrest system)
  "
[![](https://github-actions.40ants.com/40ants/cl-hamcrest/matrix.svg)](https://github.com/40ants/cl-hamcrest/actions)

![Quicklisp](http://quickdocs.org/badge/cl-hamcrest.svg)

"
  (@installation section)
  (@introduction section)
  (@why-not-patternmatching section)
  (@matchers section)
  (@prove section)
  (@roadmap section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :hamcrest)
```
""")


(defsection @introduction (:title "Introduction")
  """
This is an implementation of [Hamcrest](http://hamcrest.org/) for Common Lisp.

It simplifes unittests and make them more readable. Hamcrest uses
idea of pattern-matching, to construct matchers from different pieces and
to apply them to the data.

Here is a simple example
------------------------

```
(assert-that
  log-item
  (has-plist-entries :|@message| "Some"
                     :|@timestamp| _)
  (hasnt-plist-keys :|@fields|))
```
""")


(defsection @why-not-patternmatching (:title "Why not pattern-matching library?"
                                      :external-links (("Optima" . "https://quickdocs.org/optima")))
  """
You may ask: "Why dont use a pattern-matching library, like [Optima][Optima]?"

Here is another example from another library `log4cl-json`, where I want
to check that some fields in plist have special values and other key is not
present. Here is the data:

```
(defvar log-item '(:|@message| "Some"
                   :|@timestamp| 122434342
                   ;; this field is wrong and
                   ;; shouldn't be here
                   :|@fields| nil))
```

With [Optima][Optima] I could write this code to match the data:

```
(ok (ematch
      log-item
    ((and (guard (property :|@message| m)
                 (equal m "Some"))
          (property :|@timestamp| _)
          (not (property :|@fields| _)))
     t))
  "Log entry has message, timestamp, but not fields")
```

But error message will be quite cumbersome:

```
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
```

CL-HAMCREST is more concise and clear
-------------------------------------

With `cl-hamcrest` test becomes more readable:


```
(assert-that
      log-item
      (has-plist-entries :|@message| "Some"
                         :|@timestamp| _)
      (hasnt-plist-keys :|@fields|))
```

As well, as output about the failure:

```
× Key :|@fields| is present in object, but shouldn't
```

That is because `cl-hamcrest` tracks the context and works
together with testing framework, to output all information
to let you understand where the problem is.

Why not just use Prove's assertions?
------------------------------------

To draw a full picture, here is test, written in plain Prove's
assertions:

```
(ok (member :|@message| log-item))
(is (getf log-item :|@message|)
    "Some")
(ok (member :|@timestamp| log-item))
(ok (not (member :|@fields| log-item)))
```

And it's output:

```
✓ (:|@message| "Some") is expected to be T 
✓ "Some" is expected to be "Some" 
✓ (:|@timestamp| "2017-01-03T16:57:17.988810Z" :|@message| "Some") is expected to be T 
× NIL is expected to be T 
```

is not as clear, if you'll try to figure out
what does `NIL is expected to be T` mean.

Description of all supported matchers, you can find in the
@MATCHERS section.

""")


(defsection @roadmap (:title "Roadmap")
  """

* Logical matchers:

  - `any-of` – Matches if any of the given matchers evaluate to True.
  - `is-not` – Inverts the given matcher to its logical negation (think if
    we need it, and how to show the results, here are results
    how it works [in PyHamcrest](https://gist.github.com/svetlyak40wt/fbe480384e9e3f75b10523aa0b4fb6ce)
    – it just sees that matcher returned True and raises Assertion error with full object's content and
    matcher's description with prepended 'not' particle).

* Object matchers:

  - Add `hasnt-some-keys` matchers, corresponding to
    `has-some-entries`.
  - Make `has-alist-entries` work with keys other than keyword
    right now it uses `eql` to compare keys.

* Sequence matchers:

  - `is-in` – Matches if evaluated object is present in a given sequence.

* Other features:

  - Use uniq CommonLisp feature to restart signaled conditions to collect
    all problems with data when there are few problems with keys.

""")



