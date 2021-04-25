;; (ql:quickload :hermes-research)
(in-package :cl-user)
(defpackage hermes-research
  (:use #:cl #:postmodern)
  (:import-from #:local-time
		#:now
		#:timestamp-to-unix)
  (:import-from #:hsinp.db
		#:conn)
  (:nicknames #:hsres))
(in-package :hermes-research)

(defclass paper ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (timestamp :col-type int8 :initform (timestamp-to-unix (now)) :initarg :timestamp :accessor timestamp)
   (title :col-type string :initarg :title :accessor title)
   (archived :col-type boolean :initform nil :initarg :archived :accessor archived))
  (:metaclass dao-class)
  (:table-name papers)
  (:keys id))

(defclass paragraph ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (paper-id :col-type string :initarg :paper-id :accessor paper-id)
   (timestamp :col-type int8 :initform (timestamp-to-unix (now)) :initarg :timestamp :accessor timestamp)
   (title :col-type string :initarg :title :accessor title)
   (description :col-type string :initarg :description :accessor description)
   (idx :col-type integer :initarg :idx :accessor idx)
   (archived :col-type boolean :initform nil :initarg :archived :accessor archived))
  (:metaclass dao-class)
  (:table-name paragraphs)
  (:keys id))

(defclass sentence ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (timestamp :col-type int8 :initform (timestamp-to-unix (now)) :initarg :timestamp :accessor timestamp)
   (previous :col-type string :initform "" :initarg :previous :accessor previous)
   (next :col-type string :initform "" :initarg :next :accessor next)
   (paragraph-id :col-type string :initarg :paragraph-id :accessor paragraph-id)
   (caption-id :col-type string :initarg :caption-id :accessor caption-id)
   (idx :col-type integer :initarg :idx :accessor idx)
   (text :col-type string :initarg :text :accessor text)
   (archived :col-type boolean :initform nil :initarg :archived :accessor archived))
  (:metaclass dao-class)
  (:table-name sentences)
  (:keys id))

(defclass caption ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (text :col-type string :initarg :text :accessor text))
  (:metaclass dao-class)
  (:table-name captions)
  (:keys id))

(defclass comment ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (target-id :col-type string :initarg :target-id :accessor target-id)
   (timestamp :col-type int8 :initform (timestamp-to-unix (now)) :initarg :timestamp :accessor timestamp)
   (author :col-type string :initarg :author :accessor author)
   (text :col-type string :initarg :text :accessor text)
   (archived :col-type boolean :initform nil :initarg :archived :accessor archived))
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

(defun update-paper (paper &key title (archived nil archivedp))
  "Update paper identified by `id` with new `title`."
  (when paper
    (when title
      (setf (title paper) title))
    (when archivedp
      (setf (archived paper) archived))
    (when (or title archivedp)
      (conn (update-dao paper)))))
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
;; (id (create-paragraph (get-paper :title *paper*) :title "Meow Meow" :description "Woof Woof"))
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

(defun update-paragraph (paragraph &key idx title description (archived nil archivedp))
  (when paragraph
    (when idx
      (setf (idx paragraph) idx))
    (when title
      (setf (title paragraph) title))
    (when description
      (setf (description paragraph) description))
    (when archivedp
      (setf (archived paragraph) archived))
    (when (or idx title description archivedp)
      (conn (update-dao paragraph)))))
;; (update-paragraph (get-paragraph (get-paper :title *paper*) :idx 0) :idx 0)
;; (update-paragraph (get-paragraph (get-paper :title *paper*) :idx 1) :title "This is another title.")
;; (update-paragraph (get-paragraph (get-paper :title *paper*) :idx 1) :description "Description.")

(defun delete-paragraph (paragraph)
  (when paragraph
    (conn (delete-dao paragraph))))
;; (delete-paragraph (get-paragraph (get-paper :title *paper*) :idx 5))

(defun create-caption (caption-text)
  (when caption-text
    (conn (make-dao 'caption
		    :text caption-text))))

(defun get-caption (id)
  (when id
    (conn (get-dao 'caption id))))

(defun update-caption (id caption-text)
  (when (and id caption-text)
    (let ((caption (get-caption id)))
      (setf (text caption) caption-text)
      (conn (update-dao caption)))))

(defun delete-caption (caption)
  (conn (delete-dao caption)))

(defun create-sentence-new (paragraph &key (caption-text "") (text ""))
  (conn
   (let* ((paragraph-id (id paragraph))
	  (idx (caar (query (:select (:count 'id) :from 'sentences :where (:= 'paragraph-id paragraph-id)))))
	  (caption (create-caption caption-text)))
     (make-dao 'sentence
	       :paragraph-id paragraph-id
	       :caption-id (id caption)
	       :idx idx
	       :text text))))
;; (create-sentence-new (get-paragraph (get-paper :title *paper*) :idx 0) :caption-text "First sentence." :text "Lorem ipsum.")
;; (create-sentence-new (get-paragraph (get-paper :title *paper*) :idx 0) :caption-text "Second sentence." :text "Meow meow meow.")
;; (create-sentence-new (get-paragraph (get-paper :title *paper*) :idx 1) :caption-text "First sentence." :text "Woof woof.")

(defun create-sentence-next (previous-sentence &key caption-text (text ""))
  (conn
   (let* ((caption (create-caption (if caption-text
				       caption-text
				       (text (get-caption (caption-id previous-sentence))))))
	  (next (make-dao 'sentence
			  :paragraph-id (paragraph-id previous-sentence)
			  :caption-id (if caption
					  (id caption)
					  (caption-id previous-sentence))
			  :previous (id previous-sentence)
			  :idx (idx previous-sentence)
			  :text text)))
     (setf (next previous-sentence) (id next))
     (update-dao previous-sentence)
     next)))
;; (create-sentence-next (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0)) :caption-text "Some changes." :text "Something more meaningful.")
;; (create-sentence-next (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0)) :caption-text "Some more changes." :text "Something even more meaningful.")

(defun get-sentence (paragraph &key id idx)
  (cond (id (conn (get-dao 'sentence id)))
	((and paragraph idx)
	 (first (conn (query (:select '* :from 'sentences :where (:and (:= 'idx '$1)
								       (:= 'paragraph-id '$2)
								       (:= 'next "")))
			     idx
			     (id paragraph)
			     (:dao sentence)))))
	(paragraph (first (conn (query (:select '* :from 'sentences :where (:and (:= 'idx 0)
										 (:= 'paragraph-id '$1)
										 (:= 'next "")))
				       (id paragraph)
				       (:dao sentence)))))))
;; (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0))

(defun get-sentences (paragraph &key idx)
  (cond ((and paragraph idx)
	 (conn (query (:order-by (:select '* :from 'sentences :where (:and (:= 'paragraph-id '$1)
									   (:= 'idx idx)))
				 (:asc 'timestamp))
		      (id paragraph)
		      (:dao sentence))))
	(paragraph (conn (query (:order-by (:select '* :from 'sentences :where (:and (:= 'paragraph-id '$1)
										     (:= 'next "")))
					   (:asc 'idx))
				(id paragraph)
				(:dao sentence))))))
;; (loop for sentence in (get-sentences (get-paragraph (get-paper :title *paper*) :idx 0)) do (print (text sentence)))
;; (loop for sentence in (get-sentences (get-paragraph (get-paper :title *paper*) :idx 0) :idx 0) do (print (text sentence)))

(defun -get-sentence-previous-next (sentence operation)
  (let ((sentence-id (cond ((eq operation :next) (next sentence))
			   ((eq operation :previous) (previous sentence)))))
    (when (not (string= sentence-id ""))
      (get-sentence nil :id sentence-id))))

(defun get-sentence-previous (sentence)
  (-get-sentence-previous-next sentence :previous))
;; (text (nth 2 (get-sentences (get-paragraph (get-paper :title *paper*) :idx 0) :idx 0)))
;; (text (get-sentence-previous (nth 2 (get-sentences (get-paragraph (get-paper :title *paper*) :idx 0) :idx 0))))
;; (text (get-sentence-previous (get-sentence-previous (nth 2 (get-sentences (get-paragraph (get-paper :title *paper*) :idx 0) :idx 0)))))

(defun get-sentence-next (sentence)
  (-get-sentence-previous-next sentence :next))
;; (text (nth 0 (get-sentences (get-paragraph (get-paper :title *paper*) :idx 0) :idx 0)))
;; (text (get-sentence-next (nth 0 (get-sentences (get-paragraph (get-paper :title *paper*) :idx 0) :idx 0))))
;; (text (get-sentence-next (get-sentence-next (nth 0 (get-sentences (get-paragraph (get-paper :title *paper*) :idx 0) :idx 0)))))

(defun update-sentence (sentence &key caption-text idx (archived nil archivedp))
  (when sentence
    (when caption-text
      (let ((caption (get-caption (caption-id sentence))))
	(setf (text caption) caption-text)
	(conn (update-dao caption))))
    (when idx
      (let* ((paragraph (get-paragraph nil :id (paragraph-id sentence)))
	     (sentences (get-sentences paragraph :idx (idx sentence))))
	(conn
	 (loop for sentence in sentences
	       do (progn
		    (setf (idx sentence) idx)
		    (update-dao sentence))))))
    (when archivedp
      (setf (archived sentence) archived)
      (conn (update-dao sentence)))))
;; (text (get-caption (caption-id (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0)))))
;; (text (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0)))
;; (update-sentence (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0)) :caption-text "Some more changes that change everything.")
;; (update-sentence (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0) :idx 10) :idx 0)

(defun create-comment (target-id author text)
  (conn (make-dao 'comment
		  :target-id target-id
		  :author author
		  :text text)))
;; (create-comment (id (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0))) "Amaury" "Nice first sentence.")
;; (create-comment (id (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0))) "Mario" "Definitely!")
;; (create-comment (id (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0) :idx 1)) "Amaury" "Nice second sentence.")

(defun get-comment (id)
  (when id
    (conn (get-dao 'comment id))))

(defun get-comments (target-id)
  (when target-id
    (conn (query (:order-by (:select '* :from 'comments :where (:= 'target-id '$1))
			    (:asc 'timestamp))
		 target-id
		 (:dao comment)))))
;; (loop for comment in (get-comments (id (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0)))) do (print (text comment)))

(defun update-comment (comment &key (archived nil archivedp))
  (when (and comment archivedp)
    (setf (archived comment) archived)
    (conn (update-dao comment))))
;; (update-comment (first (get-comments (id (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0))))) :archived t)

(hscom.utils:comment
 (progn
   (progn (hsres.db:drop-database) (hsres.db:init-database))
   
   (defparameter *paper* "Fuzzy Grade of Membership as a Coordination Mechanism in Agent-Based Models")
   (defparameter *paper2* "Using Technical Indictors as Perception Mechanisms in Agent-Based Models")
   (create-paper *paper*)
   (create-paper *paper2*)

   (id (create-paragraph (get-paper :title *paper*) :title "Meow" :description "Woof"))
   (id (create-paragraph (get-paper :title *paper*) :title "Meow Meow" :description "Woof Woof"))
   (id (create-paragraph (get-paper :title *paper2*) :title "Tweet" :description "Growl"))

   (create-sentence-new (get-paragraph (get-paper :title *paper*) :idx 0) :caption-text "First sentence." :text "Lorem ipsum.")
   (create-sentence-new (get-paragraph (get-paper :title *paper*) :idx 0) :caption-text "Second sentence." :text "Meow meow meow.")
   (create-sentence-new (get-paragraph (get-paper :title *paper*) :idx 1) :caption-text "First sentence." :text "Woof woof.")

   ;; (sleep 1)
   (create-sentence-next (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0)) :caption-text "Some changes." :text "Something more meaningful.")
   ;; (sleep 1)
   (create-sentence-next (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0)) :caption-text "Some more changes." :text "Something even more meaningful.")

   (create-comment (id (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0))) "Amaury" "Nice first sentence.")
   (create-comment (id (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0))) "Mario" "Definitely!")
   (create-comment (id (get-sentence (get-paragraph (get-paper :title *paper*) :idx 0) :idx 1)) "Amaury" "Nice second sentence.")
   )
 )

;; (progn (hsres.db:drop-database) (hsres.db:init-database))
