(in-package :cl-user)
(defpackage hermes-research.utilities
  (:use
   :cl
   :postmodern
   :hermes-research.db)
  (:export
   :assoccess
   :init-database
   :drop-database
   ))
(in-package :hermes-research.utilities)

(defun assoccess (o k)
  (cdr (assoc k o)))

(defun init-database ()
  "Creates all the necessary tables for Hermes Research."
  (conn
   (unless (table-exists-p 'papers)
     (query (:create-table 'papers
		((id :type string)
		 (title :type string))
	      (:primary-key id)))
     (query (:create-table 'containers
		((id :type string)
		 (previous :type string)
		 (paper-id :type string)
		 (idx :type int))
	      (:primary-key id)))
     (query (:create-table 'units
		((id :type string)
		 (previous :type string)
		 (container-id :type string)
		 (idx :type int)
		 (title :type string)
		 (text :type string)
		 (description :type string))
	      (:primary-key id)))
     (query (:create-table 'comments
		((id :type string)
		 (unit-id :type string)
		 (timestamp :type int8)
		 (author :type string)
		 (text :type string))
	      (:primary-key id)))
     )))
;; (init-database)

(defun drop-database ()
  (conn
   (drop-table 'papers)
   (drop-table 'containers)
   (drop-table 'units)
   (drop-table 'comments)))
;; (drop-database)
