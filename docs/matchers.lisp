(uiop:define-package #:hamcrest-docs/matchers
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:docs-config
                #:docs-config))
(in-package #:hamcrest-docs/matchers)

(in-readtable pythonic-string-syntax)


(defsection @matchers (:title "Matchers library")
  "Here you will find all matchers, supported by CL-HAMCREST, grouped by
their purpose."
  (@object section)
  (@sequence section)
  (@boolean section)
  (@utils section))


(defsection @object (:title "Object matchers")
  "This kind of matchers checks some sort of properties on an object. In
this case objects are not only the CLOS objects but also, hashmaps,
alists and property lists."
  (hamcrest/matchers:has-plist-entries macro)
  (hamcrest/matchers:hasnt-plist-keys macro)
  (hamcrest/matchers:has-alist-entries macro)
  (hamcrest/matchers:has-hash-entries macro)
  (hamcrest/matchers:has-properties macro)
  (hamcrest/matchers:has-slots macro)
  (hamcrest/matchers:has-type function))

(defsection @sequence (:title "Sequence matchers")
  (hamcrest/matchers:has-length function)
  (hamcrest/matchers:contains macro)
  (hamcrest/matchers:contains-in-any-order macro))

(defsection @boolean (:title "Boolean matchers")
  (hamcrest/matchers:any function)
  (hamcrest/matchers:has-all function))

(defsection @utils (:title "Utility functions")
  ;; Probably we can return "symbol-macro" here after this bug will be fixed:
  ;; https://github.com/40ants/doc/issues/21
  (hamcrest/matchers:_ function)
  (hamcrest/matchers:matcher-description function)
  (hamcrest/matchers:matcher-form function)
  (hamcrest/matchers:assertion-error class)
  (hamcrest/matchers:assertion-error-reason (reader hamcrest/matchers:assertion-error))
  (hamcrest/matchers:assertion-context (reader hamcrest/matchers:assertion-error))
  (hamcrest/matchers:assertion-error-reason-with-context function))
