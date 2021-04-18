(defsystem "hermes-research-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Amaury Hernandez-Aguila"
  :license ""
  :depends-on ("hermes-research"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "hermes-research"))))
  :description "Test system for hermes-research"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
