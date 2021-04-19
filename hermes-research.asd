(defsystem "hermes-research"
  :version "0.1.0"
  :author "Amaury Hernandez-Aguila"
  :license ""
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"

               ;; for @route annotation
               "cl-syntax-annot"

               ;; HTML Template
               "djula"

               ;; for DB
	       "datafly"
	       "postmodern"
	       
	       ;;
	       "hermes-common"
	       )
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
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
