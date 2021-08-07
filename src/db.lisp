(in-package :cl-user)
(defpackage hermes-research.db
  (:use #:cl #:postmodern)
  (:import-from #:hscom.db
		#:conn)
  (:export #:init-database
	   #:drop-database)
  (:nicknames :hsres.db))
(in-package :hermes-research.db)

(defun init-database ()
  "Creates all the necessary tables for Hermes Research."
  (conn
   (unless (table-exists-p 'papers)
     (query (:create-table 'papers
		((id :type string)
		 (timestamp :type int8)
		 (title :type string)
		 (archived :type boolean))
	      (:primary-key id)))
     (query (:create-table 'sentences
		((id :type string)
		 (timestamp :type int8)
		 (previous :type string)
		 (next :type string)
		 (paper-id :type string)
		 (description-id :type string)
		 (idx :type int)
		 (text :type string)
		 (archived :type boolean))
		(:primary-key id)))
     (query (:create-table 'descriptions
		((id :type string)
		 (text :type string))
	      (:primary-key id)))
     (query (:create-table 'comments
		((id :type string)
		 (sentence-id :type string)
		 (timestamp :type int8)
		 (author :type string)
		 (text :type string)
		 (archived :type boolean))
	      (:primary-key id)))
     )))
;; (init-database)

(defun drop-database ()
  (conn
   (drop-table 'papers)
   (drop-table 'sentences)
   (drop-table 'descriptions)
   (drop-table 'comments)))
;; (drop-database)
