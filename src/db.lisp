(in-package :cl-user)
(defpackage hermes-research.db
  (:use #:cl #:postmodern)
  (:import-from #:hsinp.db
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
		 (title :type string))
	      (:primary-key id)))
     (query (:create-table 'paragraphs
		((id :type string)
		 (paper-id :type string)
		 (timestamp :type int8)
		 (title :type string)
		 (description :type string)
		 (idx :type int))
	      (:primary-key id)))
     (query (:create-table 'sentences
		((id :type string)
		 (timestamp :type int8)
		 (previous :type string)
		 (next :type string)
		 (paragraph-id :type string)
		 (caption-id :type string)
		 (idx :type int)
		 (text :type string))
		(:primary-key id)))
     (query (:create-table 'captions
		((id :type string)
		 (text :type string))
	      (:primary-key id)))
     (query (:create-table 'comments
		((id :type string)
		 (target-id :type string)
		 (timestamp :type int8)
		 (author :type string)
		 (text :type string))
	      (:primary-key id)))
     )))
;; (init-database)

(defun drop-database ()
  (conn
   (drop-table 'papers)
   (drop-table 'paragraphs)
   (drop-table 'sentences)
   (drop-table 'captions)
   (drop-table 'comments)))
;; (drop-database)
