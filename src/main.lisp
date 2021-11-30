;; (ql:quickload :hermes-research)
(in-package :cl-user)
(defpackage hermes-research
  (:use #:cl #:postmodern)
  (:import-from #:local-time
                #:now
                #:timestamp-to-unix)
  (:import-from #:hsinp.db
                #:conn)
  (:export #:create-paper
           #:get-paper
           #:get-papers
           #:update-paper
           #:create-description
           #:get-description
           #:update-description
           #:create-sentence-new
           #:create-sentence-next
           #:get-sentence
           #:get-sentences
           #:get-sentence-previous
           #:get-sentence-next
           #:update-sentence
           #:create-comment
           #:get-comment
           #:get-comments
           #:update-comment)
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

(defclass sentence ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (timestamp :col-type int8 :initform (timestamp-to-unix (now)) :initarg :timestamp :accessor timestamp)
   (previous :col-type string :initform "" :initarg :previous :accessor previous)
   (next :col-type string :initform "" :initarg :next :accessor next)
   (paper-id :col-type string :initarg :paper-id :accessor paper-id)
   (description-id :col-type string :initarg :description-id :accessor description-id)
   (idx :col-type integer :initarg :idx :accessor idx)
   (text :col-type string :initarg :text :accessor text)
   (archived :col-type boolean :initform nil :initarg :archived :accessor archived))
  (:metaclass dao-class)
  (:table-name sentences)
  (:keys id))

(defclass description ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (text :col-type string :initarg :text :accessor text))
  (:metaclass dao-class)
  (:table-name descriptions)
  (:keys id))

(defclass comment ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (sentence-id :col-type string :initarg :sentence-id :accessor sentence-id)
   (timestamp :col-type int8 :initform (timestamp-to-unix (now)) :initarg :timestamp :accessor timestamp)
   (author :col-type string :initarg :author :accessor author)
   (text :col-type string :initarg :text :accessor text)
   (archived :col-type boolean :initform nil :initarg :archived :accessor archived))
  (:metaclass dao-class)
  (:table-name comments)
  (:keys id))

;; (defparameter *paper* "Fuzzy Grade of Membership as a Coordination Mechanism in Agent-Based Models")
;; (defparameter *paper2* "Using Technical Indictors as Perception Mechanisms in Agent-Based Models")
;; (defparameter *paper3* "Using Fuzzy Systems for the Identification of Trading Patterns in Financial Markets")

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

(defun get-papers ()
  (conn (query (:select '* :from 'papers)
               (:dao paper))))
;; (get-papers)

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

(defun create-description (description-text)
  (when description-text
    (conn (make-dao 'description
                    :text description-text))))

(defun get-description (id)
  (when id
    (conn (get-dao 'description id))))

(defun update-description (description &key (description-text ""))
  (when description
    (setf (text description) description-text)
    (conn (update-dao description))))

(defun delete-description (description)
  (conn (delete-dao description)))

(defun create-sentence-new (paper &key (description-text "") (text ""))
  (conn
   (let* ((paper-id (id paper))
          (idx (caar (query (:select (:count 'id) :from 'sentences :where (:= 'paper-id paper-id)))))
          (description (create-description description-text)))
     (make-dao 'sentence
               :paper-id paper-id
               :description-id (id description)
               :idx idx
               :text text))))
;; (create-sentence-new (get-paper :title *paper*) :description-text "First sentence." :text "Lorem ipsum.")

(defun create-sentence-next (previous-sentence &key description-text (text ""))
  (conn
   (let* ((description (create-description (if description-text
                                               description-text
                                               (text (get-description (description-id previous-sentence))))))
          (next (make-dao 'sentence
                          :paper-id (paper-id previous-sentence)
                          :description-id (if description
                                              (id description)
                                              (description-id previous-sentence))
                          :previous (id previous-sentence)
                          :idx (idx previous-sentence)
                          :text text)))
     (setf (next previous-sentence) (id next))
     (update-dao previous-sentence)
     next)))
;; (create-sentence-next (get-sentence (get-paper :title *paper*)) :description-text "Some changes." :text "Something more meaningful.")
;; (create-sentence-next (get-sentence (get-paper :title *paper*)) :description-text "Some more changes." :text "Something even more meaningful.")

(defun get-sentence (paper &key id idx)
  (cond (id (conn (get-dao 'sentence id)))
        ((and paper idx)
         (first (conn (query (:select '* :from 'sentences :where (:and (:= 'idx '$1)
                                                                  (:= 'paper-id '$2)
                                                                  (:= 'next "")))
                             idx
                             (id paper)
                             (:dao sentence)))))
        (paper (first (conn (query (:select '* :from 'sentences :where (:and (:= 'idx 0)
                                                                        (:= 'paper-id '$1)
                                                                        (:= 'next "")))
                                   (id paper)
                                   (:dao sentence)))))))
;; (get-sentence (get-paper (get-paper :title *paper*) :idx 0))

(defun get-sentences (paper &key idx)
  (cond ((and paper idx)
         (conn (query (:order-by (:select '* :from 'sentences :where (:and (:= 'paper-id '$1)
                                                                      (:= 'idx idx)))
                       (:asc 'timestamp))
                      (id paper)
                      (:dao sentence))))
        (paper (conn (query (:order-by (:select '* :from 'sentences :where (:and (:= 'paper-id '$1)
                                                                            (:= 'next "")))
                             (:asc 'idx))
                            (id paper)
                            (:dao sentence))))))
;; (loop for sentence in (get-sentences (get-paper :title *paper*)) do (print (text sentence)))
;; (loop for sentence in (get-sentences (get-paper :title *paper*) :idx 0) do (print (text sentence)))

(defun -get-sentence-previous-next (sentence operation)
  (let ((sentence-id (cond ((eq operation :next) (next sentence))
                           ((eq operation :previous) (previous sentence)))))
    (when (not (string= sentence-id ""))
      (get-sentence nil :id sentence-id))))

(defun get-sentence-previous (sentence)
  (-get-sentence-previous-next sentence :previous))
;; (text (nth 2 (get-sentences (get-paper :title *paper*) :idx 0)))
;; (text (get-sentence-previous (nth 2 (get-sentences (get-paper :title *paper*) :idx 0))))
;; (text (get-sentence-previous (get-sentence-previous (nth 2 (get-sentences (get-paper :title *paper*) :idx 0)))))

(defun get-sentence-next (sentence)
  (-get-sentence-previous-next sentence :next))
;; (text (nth 0 (get-sentences (get-paper :title *paper*) :idx 0)))
;; (text (get-sentence-next (nth 0 (get-sentences (get-paper :title *paper*) :idx 0))))
;; (text (get-sentence-next (get-sentence-next (nth 0 (get-sentences (get-paper :title *paper*) :idx 0)))))

(defun update-sentence (sentence &key description-text idx (archived nil archivedp))
  (when sentence
    (when description-text
      (let ((description (get-description (description-id sentence))))
        (setf (text description) description-text)
        (conn (update-dao description))))
    (when idx
      (let* ((paper (get-paper :id (paper-id sentence)))
             (sentences (get-sentences paper :idx (idx sentence))))
        (conn
         (loop for sentence in sentences
               do (progn
                    (setf (idx sentence) idx)
                    (update-dao sentence))))))
    (when archivedp
      (setf (archived sentence) archived)
      (conn (update-dao sentence)))))
;; (text (get-description (description-id (get-sentence (get-paper (get-paper :title *paper*) :idx 0)))))
;; (text (get-sentence (get-paper (get-paper :title *paper*) :idx 0)))
;; (update-sentence (get-sentence (get-paper (get-paper :title *paper*) :idx 0)) :description-text "Some more changes that change everything.")
;; (update-sentence (get-sentence (get-paper (get-paper :title *paper*) :idx 0) :idx 10) :idx 0)

(defun create-comment (sentence-id author text)
  (conn (make-dao 'comment
                  :sentence-id sentence-id
                  :author author
                  :text text)))
;; (create-comment (id (get-sentence (get-paper :title *paper*))) "Amaury" "Nice first sentence.")
;; (create-comment (id (get-sentence (get-paper :title *paper*))) "Mario" "Definitely!")
;; (create-comment (id (get-sentence (get-paper :title *paper*) :idx 1)) "Amaury" "Nice second sentence.")

(defun get-comment (id)
  (when id
    (conn (get-dao 'comment id))))

(defun get-comments (sentence-id)
  (when sentence-id
    (conn (query (:order-by (:select '* :from 'comments :where (:= 'sentence-id '$1))
                  (:asc 'timestamp))
                 sentence-id
                 (:dao comment)))))
;; (loop for comment in (get-comments (id (get-sentence (get-paper :title *paper*)))) do (print (text comment)))

(defun update-comment (comment &key (archived nil archivedp))
  (when (and comment archivedp)
    (setf (archived comment) archived)
    (conn (update-dao comment))))
;; (update-comment (first (get-comments (id (get-sentence (get-paper :title *paper*))))) :archived t)

(hscom.utils:comment
 (progn
   (progn (hsres.db:drop-database) (hsres.db:init-database))

   (defparameter *paper* "Fuzzy Grade of Membership as a Coordination Mechanism in Agent-Based Models")
   (defparameter *paper2* "Using Technical Indictors as Perception Mechanisms in Agent-Based Models")
   (create-paper *paper*)
   (create-paper *paper2*)

   (create-sentence-new (get-paper :title *paper*) :description-text "First sentence." :text "Lorem ipsum.")
   (create-sentence-new (get-paper :title *paper*) :description-text "Second sentence." :text "Meow meow meow.")
   (create-sentence-new (get-paper :title *paper*) :description-text "Third sentence." :text "Woof woof.")

   ;; (sleep 1)
   (create-sentence-next (get-sentence (get-paper :title *paper*)) :description-text "Some changes." :text "Something more meaningful.")
   ;; (sleep 1)
   (create-sentence-next (get-sentence (get-paper :title *paper*)) :description-text "Some more changes." :text "Something even more meaningful.")

   (create-comment (id (get-sentence (get-paper :title *paper*))) "Amaury" "Nice first sentence.")
   (create-comment (id (get-sentence (get-paper :title *paper*))) "Mario" "Definitely!")
   (create-comment (id (get-sentence (get-paper :title *paper*) :idx 1)) "Amaury" "Nice second sentence.")
   )
 )

;; (progn (hsres.db:drop-database) (hsres.db:init-database))
