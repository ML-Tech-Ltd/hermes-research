(defsystem "hermes-research"
    :version "0.1.0"
    :author "Amaury Hernandez-Aguila"
    :license ""
    :depends-on ( ;; DB
		 #:postmodern
		 #:uuid
		 
		 ;; Hermes ecosystem.
		 #:hermes-common
		 #:hermes-input
		 )
    :components ((:module "src"
			  :components
			  ((:file "main")
			   (:file "db")
			   (:file "utils")
			   )))
    :description ""
    :in-order-to ((test-op (test-op "hermes-research-test"))))

(defsystem "hermes-research-test"
    :defsystem-depends-on ("prove-asdf")
    :author "Amaury Hernandez-Aguila"
    :license ""
    :depends-on ("hermes-research"
		 "rove")
    :components ((:module "tests"
			  :components
			  ((:test-file "main"))))
    :description "Test system for hermes-research"
    :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
