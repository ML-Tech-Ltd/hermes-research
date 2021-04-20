;; (ql:quickload :hermes-research)
(in-package :cl-user)
(defpackage hermes-research
  (:use #:cl #:postmodern)
  (:import-from #:hsinp.db
		#:conn)
  (:nicknames #:hsres))
(in-package :hermes-research)

(defclass paper ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (title :col-type string :initarg :title :accessor title))
  (:metaclass dao-class)
  (:table-name papers)
  (:keys id))

(defclass paragraph ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (paper-id :col-type string :initarg :paper-id :accessor paper-id)
   (idx :col-type integer :initarg :idx :accessor idx)
   (title :col-type string :initarg :title :accessor title)
   (description :col-type string :initarg :description :accessor description))
  (:metaclass dao-class)
  (:table-name paragraphs)
  (:keys id))

(defclass sentence ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (paragraph-id :col-type string :initarg :paragraph-id :accessor paragraph-id)
   (previous :col-type string :initform "" :initarg :previous :accessor previous)
   (next :col-type string :initform "" :initarg :next :accessor next)
   (idx :col-type integer :initarg :idx :accessor idx)
   (title :col-type string :initarg :title :accessor title)
   (description :col-type string :initarg :description :accessor description)
   (text :col-type string :initarg :text :accessor text))
  (:metaclass dao-class)
  (:table-name sentences)
  (:keys id))

(defclass comment ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (target-id :col-type string :initarg :target-id :accessor target-id)
   (timestamp :col-type int8 :initarg :timestamp :accessor timestamp)
   (author :col-type string :initarg :author :accessor author)
   (text :col-type string :initarg :text :accessor text))
  (:metaclass dao-class)
  (:table-name comments)
  (:keys id))

;; (defparameter *paper* "Fuzzy Grade of Membership as a Coordination Mechanism in Agent-Based Models")
;; (defparameter *paper2* "Using Technical Indictors as Perception Mechanisms in Agent-Based Models")

(defun create-paper (title)
  (conn (make-dao 'paper :title title)))
;; (create-paper *paper*)
;; (create-paper *paper2*)

(defun get-paper (&key id title)
  (cond (id (conn (get-dao 'paper id)))
	(title (first (conn (query (:select '* :from 'papers :where (:= 'title title)) (:dao paper)))))))
;; (get-paper :id (hscom.utils:assoccess (conn (query (:select '* :from 'papers) :alist)) :id))
;; (get-paper :id (hscom.utils:assoccess (second (conn (query (:select '* :from 'papers) :alists))) :id))
;; (get-paper :title *paper*)
;; (get-paper :title *paper2*)

(defun update-paper (paper title)
  "Update paper identified by `id` with new `title`."
  (when title
    (setf (paper-title paper) title)
    (conn (update-dao paper))))
;; (update-paper (get-paper :title *paper*) "Meow")
;; (update-paper (get-paper :title "Meow") *paper*)

(defun delete-paper (paper)
  (when paper
    (conn (delete-dao paper))))
;; (delete-paper (get-paper :title *paper*))

(defun create-paragraph (paper &key (title "") (description ""))
  (conn
   (let* ((paper-id (id paper))
	  (idx (caar (query (:select (:count 'id) :from 'paragraphs :where (:= 'paper-id paper-id))))))
     (make-dao 'paragraph
	       :paper-id paper-id
	       :idx idx
	       :title title
	       :description description))))
;; (id (create-paragraph (get-paper :title *paper*) :title "Meow" :description "Woof"))
;; (id (create-paragraph (get-paper :title *paper2*) :title "Tweet" :description "Growl"))

(defun get-paragraph (paper &key id idx)
  (cond (id (conn (get-dao 'paragraph id)))
	((and paper idx) (first (conn (query (:select '* :from 'paragraphs :where (:and (:= 'idx '$1)
											(:= 'paper-id '$2)))
					     idx
					     (id paper)
					     (:dao paragraph)))))
	(paper (first (conn (query (:select '* :from 'paragraphs :where (:and (:= 'idx 0)
									      (:= 'paper-id '$1)))
				   (id paper)
				   (:dao paragraph)))))))
;; (get-paragraph nil :id (id (get-paragraph (get-paper :title *paper*))))
;; (get-paragraph (get-paper :title *paper*) :idx 0)

(defun update-paragraph (paragraph &key idx title description)
  (when idx
    (setf (idx paragraph) idx))
  (when title
    (setf (title paragraph) title))
  (when description
    (setf (description paragraph) description))
  (when (or idx title description)
    (conn (update-dao paragraph))))
;; (update-paragraph (get-paragraph (get-paper :title *paper*) :idx 0) :idx 0)
;; (update-paragraph (get-paragraph (get-paper :title *paper*) :idx 1) :title "This is another title.")
;; (update-paragraph (get-paragraph (get-paper :title *paper*) :idx 1) :description "Description.")

(defun delete-paragraph (paragraph)
  (when paragraph
    (conn (delete-dao paragraph))))
;; (delete-paragraph (get-paragraph (get-paper :title *paper*) :idx 5))

(defun create-sentence-new (paragraph &key (title "") (description "") (text ""))
  (conn
   (let* ((paragraph-id (id paragraph))
	  (idx (caar (query (:select (:count 'id) :from 'sentences :where (:= 'paragraph-id paragraph-id))))))
     (make-dao 'sentence
	       :paragraph-id paragraph-id
	       :previous previous
	       :next next
	       :idx idx
	       :title title
	       :description description
	       :text text))))
;; (create-sentence-new (get-paragraph (get-paper :title *paper*) :idx 0) :title "First sentence." :text "Lorem ipsum.")
;; (create-sentence-new (get-paragraph (get-paper :title *paper*) :idx 0) :title "Second sentence." :text "Meow meow meow.")
;; (create-sentence-new (get-paragraph (get-paper :title *paper*) :idx 1) :title "First sentence." :description "Second paragraph." :text "Woof woof.")

(defun create-sentence-next (sentence))

;; (progn (hsres.db:drop-database) (hsres.db:init-database))
