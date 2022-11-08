#-asdf3.1 (error "hamcrest requires ASDF 3.1")
(defsystem "hamcrest-ci"
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "New BSD License"
  :pathname "src"
  :depends-on ("hamcrest-ci/ci")
  :description "A CI config for cl-hamcrest")
