<a id="x-28HAMCREST-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# Implementation of Hamcrest for Common Lisp

<a id="hamcrest-asdf-system-details"></a>

## HAMCREST ASDF System Details

* Description: A set of helpers to make your unittests more readable by using Hamcrest assertions.
* Licence: New `BSD` License
* Author: Alexander Artemenko
* Bug tracker: [https://github.com/40ants/cl-hamcrest/issues][eafb]
* Source control: [GIT][46fe]
* Depends on: [alexandria][8236], [cl-ppcre][49b9], [iterate][7d7b], [split-sequence][3dcd]

[![](https://github-actions.40ants.com/40ants/cl-hamcrest/matrix.svg)][3156]

![](http://quickdocs.org/badge/cl-hamcrest.svg)

<a id="x-28HAMCREST-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :hamcrest)
```
<a id="x-28HAMCREST-DOCS-2FINDEX-3A-3A-40INTRODUCTION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Introduction

This is an implementation of [Hamcrest][2b96] for Common Lisp.

It simplifes unittests and make them more readable. Hamcrest uses
idea of pattern-matching, to construct matchers from different pieces and
to apply them to the data.

<a id="here-is-a-simple-example"></a>

### Here is a simple example

```
(assert-that
  log-item
  (has-plist-entries :|@message| "Some"
                     :|@timestamp| _)
  (hasnt-plist-keys :|@fields|))
```
<a id="x-28HAMCREST-DOCS-2FINDEX-3A-3A-40WHY-NOT-PATTERNMATCHING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Why not pattern-matching library?

You may ask: "Why dont use a pattern-matching library, like [Optima][d992]?"

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
With [Optima][d992] I could write this code to match the data:

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
<a id="cl-hamcrest-is-more-concise-and-clear"></a>

### CL-HAMCREST is more concise and clear

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

<a id="why-not-just-use-prove-s-assertions"></a>

### Why not just use Prove's assertions?

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
[`Matchers library`][60ce] section.

<a id="x-28HAMCREST-DOCS-2FMATCHERS-3A-3A-40MATCHERS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Matchers library

Here you will find all matchers, supported by `CL-HAMCREST`, grouped by
their purpose.

<a id="x-28HAMCREST-DOCS-2FMATCHERS-3A-3A-40OBJECT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Object matchers

This kind of matchers checks some sort of properties on an object. In
this case objects are not only the `CLOS` objects but also, hashmaps,
alists and property lists.

<a id="x-28HAMCREST-2FMATCHERS-3AHAS-PLIST-ENTRIES-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](f0a1) `hamcrest/matchers:has-plist-entries` &rest entries

Matches plist entries:

```
TEST> (let ((obj '(:foo :bar)))
        (assert-that obj
                     (has-plist-entries :foo "bar"
                                        :blah "minor")))
  × Key :FOO has :BAR value, but "bar" was expected
```
This way you can test any number of plist's entries.

<a id="x-28HAMCREST-2FMATCHERS-3AHASNT-PLIST-KEYS-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](0a83) `hamcrest/matchers:hasnt-plist-keys` &rest keys

Checks if given keys are missing from an object:

```
TEST> (let ((obj '(:foo "bar")))
        (assert-that obj
                     (hasnt-plist-keys :blah :minor)))
  ✓ Keys :BLAH, :MINOR are absent
```
Assertion fails if at least one key is present in the object:

```
TEST> (let ((obj '(:foo "bar")))
        (assert-that obj
                     (hasnt-plist-keys :blah :foo)))
  × Key :FOO is present in object, but shouldn't.
```
<a id="x-28HAMCREST-2FMATCHERS-3AHAS-ALIST-ENTRIES-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](a59e) `hamcrest/matchers:has-alist-entries` &rest entries

Matches alist entries:

```
TEST> (let ((obj '((:the-key . "value"))))
        (assert-that obj
                     (has-alist-entries :the-key "value")))

  ✓ Has alist entries:
      :THE-KEY = "value"

TEST> (let ((obj '((:the-key . "value"))))
        (assert-that obj
                     (has-alist-entries :the-key "value"
                                        :missing-key "value")))

  × Key :MISSING-KEY is missing

TEST> (let ((obj '((:the-key . "value"))))
        (assert-that obj
                     (has-alist-entries :the-key "other-value")))

  × Key :THE-KEY has "value" value, but "other-value" was expected
```
<a id="x-28HAMCREST-2FMATCHERS-3AHAS-HASH-ENTRIES-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](7747) `hamcrest/matchers:has-hash-entries` &rest entries

Matches hash entries:

```
TEST> (let ((obj (make-hash-table)))
        (setf (gethash 'the-key obj) "value")
        (assert-that obj
                     (has-hash-entries 'the-key "value")))

  ✓ Has hash entries:
      THE-KEY = "value"

TEST> (let ((obj (make-hash-table)))
        (setf (gethash 'the-key obj) "value")
        (assert-that obj
                     (has-hash-entries 'missing-key "value")))

  × Key MISSING-KEY is missing

TEST> (let ((obj (make-hash-table)))
        (setf (gethash 'the-key obj) "value")
        (assert-that obj
                     (has-hash-entries 'the-key "other-value")))

  × Key THE-KEY has "value" value, but "other-value" was expected
```
<a id="x-28HAMCREST-2FMATCHERS-3AHAS-PROPERTIES-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](7bb6) `hamcrest/matchers:has-properties` &rest entries

Matches object properties:

```
TEST> (defvar the-object)
THE-OBJECT
TEST> (setf (getf the-object :tags) '(one two))
TEST> (assert-that 'the-object
                   (has-properties :tags '(one two)))
  ✓ Has properties:
      :TAGS = (ONE TWO)

TEST> (assert-that 'the-object
                   (has-properties :tags 'wrong-value))
  × Property :TAGS has (ONE TWO) value, but WRONG-VALUE was expected

TEST> (assert-that 'the-object
                   (has-properties :missing-property '(one two)))
  × Property :MISSING-PROPERTY is missing
```
<a id="x-28HAMCREST-2FMATCHERS-3AHAS-SLOTS-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](f096) `hamcrest/matchers:has-slots` &rest entries

Matches object slots:

```
TEST> (defstruct task
        title
        description)

TEST> (defvar task (make-task :title "The title "))

TEST> (assert-that task
                   (has-slots 'title "The title "))
  ✓ Has slots:
      TITLE = "The title "

TEST> (assert-that task
                   (has-slots 'title "Wrong title "))
  × Slot TITLE has "The title " value, but "Wrong title " was expected

TEST> (assert-that task
                   (has-slots 'description nil))
  ✓ Has slots:
      DESCRIPTION = NIL
```
<a id="x-28HAMCREST-2FMATCHERS-3AHAS-TYPE-20FUNCTION-29"></a>

#### [function](5cfe) `hamcrest/matchers:has-type` expected-type

Checks if a list have specivied length.

```
TEST> (matcher-description (has-type 'cons))
"Has type CONS"

TEST> (funcall (has-type 'cons) 100500)
; Debugger entered on #<ASSERTION-ERROR 100500 has type (INTEGER 0 4611686018427387903), but CONS was expected>
```
<a id="x-28HAMCREST-DOCS-2FMATCHERS-3A-3A-40SEQUENCE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Sequence matchers

<a id="x-28HAMCREST-2FMATCHERS-3AHAS-LENGTH-20FUNCTION-29"></a>

#### [function](ad36) `hamcrest/matchers:has-length` expected-length

Checks if a list have specivied length.

```
TEST> (assert-that 'nil (has-length 0))
  ✓ Has length of 0

TEST> (assert-that '(a b c d) (has-length 4))
  ✓ Has length of 4

TEST> (assert-that '(a b c d) (has-length 100500))
  × List (A B C D) has length of 4, but 100500 was expected
```
<a id="x-28HAMCREST-2FMATCHERS-3ACONTAINS-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](8e69) `hamcrest/matchers:contains` &rest entries

Checks if each item from a list matches to given matchers.

Contains can accept as raw values, as another matchers:

```
TEST> (assert-that '(:foo
                     (a b c)
                     d)
                   (contains :foo
                             (has-length 3)
                             'd))
  ✓ Contains all given values
```
Given list should have a length equal to count of matchers:

```
TEST> (assert-that '(:foo
                     (a b c)
                     d)
                   (contains :foo))
  × Expected value is shorter than result
```
You can ignore value of some list items, by using `(any)` matcher:

```
TEST> (assert-that '(:foo
                     (a b c)
                     d)
                   (contains :foo (any) (any)))
  ✓ Contains all given values
```
<a id="x-28HAMCREST-2FMATCHERS-3ACONTAINS-IN-ANY-ORDER-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](b896) `hamcrest/matchers:contains-in-any-order` &rest entries

Same as `contains`, but items in the sequence can be in any order:

```
TEST> (assert-that '(:foo
                     (a b c)
                     d)
                   (contains-in-any-order
                    (has-length 3)
                    'd
                    :foo))

  ✓ Contains all given values
```
<a id="x-28HAMCREST-DOCS-2FMATCHERS-3A-3A-40BOOLEAN-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Boolean matchers

<a id="x-28HAMCREST-2FMATCHERS-3AANY-20FUNCTION-29"></a>

#### [function](ac51) `hamcrest/matchers:any`

Assertion is passed regardles of value of the object:

```
TEST> (assert-that 1 (any))
  ✓ Any value if good enough

TEST> (assert-that "the-string" (any))
  ✓ Any value if good enough

TEST> (assert-that 'the-symbol (any))
  ✓ Any value if good enough

TEST> (assert-that '(1 2 3) (any))
  ✓ Any value if good enough
```
<a id="x-28HAMCREST-2FMATCHERS-3AHAS-ALL-20FUNCTION-29"></a>

#### [function](6504) `hamcrest/matchers:has-all` &rest matchers

Makes a matcher which groups another matchers with `AND` logic.

This way we can check if plist has one key and hasn't another.
And if all matchers succeed, then `has-all` succeed as well:

```
TEST> (assert-that '(:foo "bar")
                   (has-all (has-plist-entries :foo "bar")
                            (hasnt-plist-keys :blah)))
  ✓ All checks are passed
```
If at least one check is failed, then `has-all` fails too:

```
TEST> (assert-that '(:foo "bar" :blah "minor")
                   (has-all (has-plist-entries :foo "bar")
                            (hasnt-plist-keys :blah)))
  × Key :BLAH is present in object, but shouldn't
```
<a id="x-28HAMCREST-DOCS-2FMATCHERS-3A-3A-40UTILS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Utility functions

<a id="x-28HAMCREST-2FMATCHERS-3A-5F-20FUNCTION-29"></a>

#### [function](d4e6) `hamcrest/matchers:_`

Symbol _ should be used as is not as a function.

<a id="x-28HAMCREST-2FMATCHERS-3AMATCHER-DESCRIPTION-20FUNCTION-29"></a>

#### [function](408c) `hamcrest/matchers:matcher-description` fn

Returns description of a given matcher function.

Can be used to print nested matchers in a nicely indented,
human readable way:

```
TEST> (matcher-description (has-length 100500))
"Has length of 100500 "

TEST> (matcher-description (contains
                            (has-plist-entries :foo "bar ")
                            (has-plist-entries :foo "minor ")))
"Contains all given values "

TEST> (matcher-description (has-plist-entries
                            :foo "bar "
                            :blah (has-hash-entries :minor "again ")))
"Has plist entries:
  :FOO = "bar"
  :BLAH = Has hash entries:
            :MINOR = "again""
```
<a id="x-28HAMCREST-2FMATCHERS-3AMATCHER-FORM-20FUNCTION-29"></a>

#### [function](db4f) `hamcrest/matchers:matcher-form` fn

Returns description of a given matcher function.

Can be used to print nested matchers in a nicely indented,
human readable way:

```
TEST> (matcher-description (has-length 100500))
"Has length of 100500 "

TEST> (matcher-description (contains
                            (has-plist-entries :foo "bar ")
                            (has-plist-entries :foo "minor ")))
"Contains all given values "

TEST> (matcher-description (has-plist-entries
                            :foo "bar "
                            :blah (has-hash-entries :minor "again ")))
"Has plist entries:
  :FOO = "bar"
  :BLAH = Has hash entries:
            :MINOR = "again""
```
<a id="x-28HAMCREST-2FMATCHERS-3AASSERTION-ERROR-20CONDITION-29"></a>

#### [condition](b31e) `hamcrest/matchers:assertion-error` (error)

<a id="x-28HAMCREST-2FMATCHERS-3AASSERTION-ERROR-REASON-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20HAMCREST-2FMATCHERS-3AASSERTION-ERROR-29-29"></a>

#### [reader](b31e) `hamcrest/matchers:assertion-error-reason` (assertion-error) (:reason)

<a id="x-28HAMCREST-2FMATCHERS-3AASSERTION-CONTEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20HAMCREST-2FMATCHERS-3AASSERTION-ERROR-29-29"></a>

#### [reader](b31e) `hamcrest/matchers:assertion-context` (assertion-error) (= '(copy-list \*context\*))

<a id="x-28HAMCREST-2FMATCHERS-3AASSERTION-ERROR-REASON-WITH-CONTEXT-20FUNCTION-29"></a>

#### [function](a601) `hamcrest/matchers:assertion-error-reason-with-context` condition &key (indent-spaces 2)

Returns a multiline string where error reason is nested into the context
like that:

Item with index 1:
  Alist entry with key `:NAME`
    Alist entry with key `:FIRST` is required

Parameter :indent-spaces could be specified to control number of spaces
for each indentation level.

<a id="x-28HAMCREST-DOCS-2FPROVE-3A-3A-40PROVE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Integration with Prove

`CL-HAMCREST` has integration with [Prove][653f].

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

All available matchers are described in the hamcrest-docs/matchers:@matchers section.

<a id="x-28HAMCREST-2FPROVE-3AASSERT-THAT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

### [macro](ef78) `hamcrest/prove:assert-that` value &rest matchers

Main macro to test values agains matchers.

<a id="x-28HAMCREST-DOCS-2FINDEX-3A-3A-40ROADMAP-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Roadmap

* Logical matchers:
* `any-of` – Matches if any of the given matchers evaluate to True.
* `is-not` – Inverts the given matcher to its logical negation (think if
we need it, and how to show the results, here are results
how it works [in PyHamcrest][931c]
– it just sees that matcher returned True and raises Assertion error with full object's content and
matcher's description with prepended 'not' particle).
* Object matchers:
* Add `hasnt-some-keys` matchers, corresponding to
`has-some-entries`.
* Make `has-alist-entries` work with keys other than keyword
right now it uses `eql` to compare keys.
* Sequence matchers:
* `is-in` – Matches if evaluated object is present in a given sequence.
* Other features:
* Use uniq CommonLisp feature to restart signaled conditions to collect
all problems with data when there are few problems with keys.


[60ce]: #x-28HAMCREST-DOCS-2FMATCHERS-3A-3A-40MATCHERS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[2b96]: http://hamcrest.org/
[931c]: https://gist.github.com/svetlyak40wt/fbe480384e9e3f75b10523aa0b4fb6ce
[46fe]: https://github.com/40ants/cl-hamcrest
[3156]: https://github.com/40ants/cl-hamcrest/actions
[b31e]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L123
[a601]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L141
[d4e6]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L224
[f0a1]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L339
[a59e]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L367
[7747]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L408
[408c]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L44
[7bb6]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L450
[f096]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L487
[0a83]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L533
[ac51]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L575
[6504]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L602
[ad36]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L645
[8e69]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L681
[db4f]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L76
[b896]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L764
[5cfe]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/matchers.lisp#L828
[ef78]: https://github.com/40ants/cl-hamcrest/blob/cd17c96d7e6c1ee5d876ddc2bf63133862989476/src/prove.lisp#L43
[eafb]: https://github.com/40ants/cl-hamcrest/issues
[653f]: https://github.com/fukamachi/prove
[8236]: https://quickdocs.org/alexandria
[49b9]: https://quickdocs.org/cl-ppcre
[7d7b]: https://quickdocs.org/iterate
[d992]: https://quickdocs.org/optima
[3dcd]: https://quickdocs.org/split-sequence

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
