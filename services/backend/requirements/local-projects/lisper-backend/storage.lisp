(in-package :lisper-backend)
;;
;; low-level database functions:
;; init-storage, escape-sql, execute-sql
;;

(define-symbol-macro *pool* (make-instance 'radiance-pool:pool))

(defvar *init* nil)

(defvar *db-pw*
  (with-open-file
      (s "./db-password.txt"
	 :direction :input
	 :if-does-not-exist nil)
    (if s (read s) "")))

(defvar *smtp-pw*
  (with-open-file
      (s "./smtp-password.txt"
	 :direction :input
	 :if-does-not-exist nil)
    (if s (read s) "")))

(defun init-storage ()
  (radiance-pool:initialize
   :db-host "lisperch-storage"
   :db-name "ekagra"
   :db-user "ekagra"
   :db-password *db-pw*)
  (setf *init* t))

(defun escape-sql (sql)
  (let ((apices (count #\' sql)))
    (if (not (zerop apices))
	(let*
	    ((source-length (length sql))
	     (target-length (+ source-length apices))
	     (result (make-string target-length)))
	  (loop for i from 0 below source-length
		with skip = 0
		do
		   (setf (aref result (+ skip i)) (aref sql i))
		   (when (eq (aref sql i) #\')
		     (incf skip)
		     (setf (aref result (+ skip i)) #\')))
	  result)
	sql)))

(lisp-unit:define-test escape-sql
  (let ((not-escapable "pippo smith")
	(escapable "pippo 'mad dog' smith"))
    (lisp-unit:assert-equalp not-escapable (escape-sql not-escapable))
    (lisp-unit:assert-equalp "pippo ''mad dog'' smith"
			     (escape-sql escapable))))

(defun execute-sql (q)
  (when (null *init*)
    (init-storage))
  (radiance-pool:pooled-query q))

;;
;; API utilities:
;; id-for-credentials, nickname-for-id
(defun id-for-credentials (email password)
  (let ((result
	  (execute-sql
	   (format nil
		   "select id from public.utenti 
 where lower(email)='~a' and password='~a';"
		   (string-downcase email) password))))
    (when result
      (cdr (caar result)))))

(defun nickname-for-id (user-id)
  (let ((result
	  (execute-sql
	   (format nil
		   "select nickname from public.utenti  where id = ~d;"
		   user-id))))
    (when result
      (cdr (caar result)))))

;;
;; Session utilities:
;; get-session-duration-minutes close-all-expired-sessions
;; has-open-sessions, create-session, close-session,
;; valid-session-p

(defparameter *session-minutes* 2)

(defun get-session-duration-minutes()
  *session-minutes*)

(defun close-all-expired-sessions()
  (execute-sql
   (format
    nil
    "update sessioni 
 set chiusa = current_timestamp 
 where chiusa is null
 and durata_sessione_valida(id,~d) is false;"
    (get-session-duration-minutes))))

(defun has-open-sessions(user-id)
  (let ((sessions
	  (execute-sql
	   (format nil
		   "select id from sessioni where utente=~d and not chiusa and  
 (inizio, inizio + interval '~d minutes') overlaps (current_timestamp, current_timestamp + interval '~d minutes')"
		   user-id *session-minutes* *session-minutes*))))
    sessions))

(defun create-session (user-id remote-addr)
  (close-all-expired-sessions)
  (let ((result
	  (execute-sql
	   (format nil
		   "insert into sessioni (remoto, utente) values 
 ('~a', ~d) returning token;"
		   remote-addr user-id))))
    (cdr (caar result))))

(defun close-session (token)
  (let
      ((sql-command
	 (format nil
	   "update public.sessioni set chiusa = current_timestamp where token = '~a';"
	   token)))
    (log-message* :info "SQL: ~s~%" sql-command)
    (Execute-sql sql-command)))


(defun valid-session-p(token)
  (let ((sessions
	  (execute-sql
	   (format nil
		   "select id from sessioni 
 where token = '~a' 
 and chiusa is null 
 and durata_sessione_valida(id, ~d);"
		   token *session-minutes*))))
    sessions))



(defun register-user (email nickname password)
  (let
      ((exists
	 (execute-sql
	  (format nil
		  "select id from utenti where lower(email)='~a';"
		  (string-downcase email)))))
    (when (not exists)
      (let
	  ((result
	     (cdaar
	      (execute-sql
	       (format nil
		       "insert into utenti 
 (email, nickname, password) 
 values ('~a','~a','~a') returning id;"
		       email nickname password)))))
	(when result
	  (cl-smtp:send-email
	   "mail.lisper.ch"
	   "non-rispondere-ekagra-yoga@lisper.ch"
	   email
	   "La tua registrazione su Ekagra Yoga"
	   (format nil
		   "Abbiamo ricevuto una richiesta di iscrizione a ~% 
Ekagra Yoga - scuola di Yoga online ~% 
a nome di ~a~%
Se vuoi confermare l'iscrizione, per favore apri questo link:~%
https://lisper.ch/#/confirm?pending=~d"
		   email
		   result)
	   :authentication
	   '("salvatore.uras@lisper.ch" *smtp-pw*))
	  T)))))
	 
(defun confirm-user(id)
  (let* ((sql-command
	  (format nil
		  "update utenti set abilitato = current_timestamp 
 where id = ~d returning id" id))
	 (result (execute-sql sql-command)))
    result))
    
(defun extend-session(token)
  (let
      ((result
	 (execute-sql
	  (format
	   nil
	   "update sessioni set proroga = current_timestamp 
 where token = '~a' 
 and chiusa is null 
 returning id"
	   token))))
    result))


(defun enable-subscriber (email)
  (execute-sql
   (format nil
	   "update public.utenti set abbonato = current_timestamp 
 where email = '~a'" email)))

(defun disable-subscriber (email)
  (execute-sql
   (format nil
	   "update public.utenti set abbonato = null
 where email = '~a'" email)))

;;
;; Utilities
;;
(defmacro get-column (row key)
  `(cdr (assoc ,key ,row :test 'equalp)))


(lisp-unit:define-test test-json-def
  (let ((jd (make-instance 'json-def)))
    (lisp-unit:assert-equal
     0
     (second (multiple-value-list (write-json-def-path jd '("atom") 0))))
    (lisp-unit:assert-equal
     0
     (second (multiple-value-list (read-json-def-path jd '("atom")))))
    (lisp-unit:assert-equal
     'hash-table
     (type-of
      (second (multiple-value-list (+subtree-json-def-path jd '("subtree"))))))
    (lisp-unit:assert-equal
     'hash-table
     (type-of
      (second (multiple-value-list (read-json-def-path jd '("subtree"))))))
    (lisp-unit:assert-equal
     42
     (second
      (multiple-value-list (write-json-def-path jd '("subtree" "atom") 42))))
    (lisp-unit:assert-equal
     42
     (second (multiple-value-list (read-json-def-path jd '("subtree" "atom")))))
    (lisp-unit:assert-equal
     '(simple-vector 3)
     (type-of
      (second
       (multiple-value-list (+array-json-def-path jd '("array-subtree") 3))))) 
    (lisp-unit:assert-equal
     9
     (second
      (multiple-value-list (write-json-def-path jd '("array-subtree" 0) 9))))
    (lisp-unit:assert-equal
     9
     (second (multiple-value-list (read-json-def-path jd '("array-subtree" 0)))))
    (lisp-unit:assert-equal
     12
     (second
      (multiple-value-list (write-json-def-path jd '("array-subtree" 1) 12))))
    (lisp-unit:assert-equal
     12
     (second (multiple-value-list (read-json-def-path jd '("array-subtree" 1)))))
    (lisp-unit:assert-equal
     1963
     (second
      (multiple-value-list (write-json-def-path jd '("array-subtree" 2) 1963))))
    (lisp-unit:assert-equal
     1963
     (second (multiple-value-list (read-json-def-path jd '("array-subtree" 2)))))
    (lisp-unit:assert-equal
     'hash-table
     (type-of
      (second
       (multiple-value-list (+subtree-json-def-path jd '("array-subtree" 1))))))
    (lisp-unit:assert-true
     (equalp
      "Salvatore"
      (second
       (multiple-value-list
	(write-json-def-path jd '("array-subtree" 1 "nome") "Salvatore")))))
    (princ (cl-json:encode-json-to-string (root jd)))))

(defclass json-def ()
  ((root 
    :type hash-table
    :reader root
    :initform (make-hash-table :test 'equalp))))

(defun $cross-jd$ (root path &key write new-value write-op)
  (block nil
    (cond
     ((and (null path) write) ; fine path, scrittura
      (when (null write-op) ; errore: write-op nulla
        (return (values nil (format nil "invalid write operation"))))
      (funcall write-op)
      (return (values t new-value))) ; => valore scritto

     ((null path) ; fine path, lettura
      (return (values T root))) ; => root

     ((stringp (first path))
      (when (not (eq (type-of root) 'hash-table))
        (return
	  (values nil
		  (format nil
			  "invalid key ~s on non-hash-table node ~a"
			  (first path) root))))
      (let
          ((new-root-def (multiple-value-list (gethash (first path) root)))
           (write-op
	     (when (and write (null (rest path)))
	       (compile nil
			(lambda()
			  (setf (gethash (first path) root) new-value))))))
        (if write
            (return ($cross-jd$ (first new-root-def) (rest path)
				:write t
				:new-value new-value
				:write-op write-op))
          (if (null (second new-root-def))
              (return (values nil (format nil "nonexisting key ~s" (first path))))
            (return ($cross-jd$ (first new-root-def) (rest path)))))))

     ((integerp (first path))
      (when (not (arrayp root))
        (return (values nil (format nil "invalid subscript ~d on non-array node ~a" (first path) root))))
      (when (>= (first path) (length root))
        (return (values nil (format nil "subscript ~d on array length ~d" (first path) (length root)))))
      (let
          ((new-root (aref root (first path)))
           (write-op
	     (when (and write (null (rest path)))
	       (compile nil
			(lambda() (setf (aref root (first path)) new-value))))))
        (if write
            (return ($cross-jd$ new-root (rest path)
				:write t
				:new-value new-value
				:write-op write-op))
          (return ($cross-jd$ new-root (rest path)))))))))

(defmethod write-json-def-path ((jd json-def)(path list) new-value)
  ($cross-jd$ (root jd) path :write T :new-value new-value))

(defmethod +subtree-json-def-path ((jd json-def)(path list))
  ($cross-jd$ (root jd) path :write T :new-value (make-hash-table :test 'equalp)))

(defmethod +array-json-def-path ((jd json-def)(path list)(size integer))
  ($cross-jd$ (root jd) path :write T :new-value (make-array `(,size))))

(defmethod read-json-def-path ((jd json-def)(path list))
  ($cross-jd$ (root jd) path))

(defun test-json-def()
  (lisp-unit:run-tests '(test-json-def)))

;;
;; Content list
;;


(defun list-content(&key private)
  (let*
      ((sql-list-video
	 (format nil
		 "select video.nome, 
 ~a as link, video.pubblico, 
 corsi.nome as nome_corso, 
 sezioni.nome as nome_sezione from video, corsi, sezioni 
 where video.corso = corsi.id and corsi.sezione=sezioni.id ~a 
 order by sezioni.sequenza,corsi.livello, video.sequenza, video.nome;"
		 (if private "video.link"
		     "case when video.pubblico=true then video.link else 'ristretto' end")
		 (if private ""
		     "and sezioni.pubblico=true and corsi.pubblico=true")))
       (sql-list-articoli
	 (format nil
		 "select articoli.nome, 
 ~a as contenuto, articoli.pubblico, 
 corsi.nome as nome_corso, 
 sezioni.nome as nome_sezione from articoli, corsi, sezioni 
 where articoli.corso = corsi.id and corsi.sezione=sezioni.id ~a 
 order by sezioni.sequenza,corsi.livello, articoli.sequenza, articoli.nome;"
		  (if private "articoli.contenuto"
		      "case when articoli.pubblico=true then articoli.contenuto else 'ristretto' end")
		  (if private ""
		      "and sezioni.pubblico=true and corsi.pubblico=true")))
       (rows-sezioni (list-sezioni :private private))
       (rows-corsi (list-corsi :private private))
       (rows-video
	 (execute-sql sql-list-video))
       (rows-articoli
	 (execute-sql sql-list-articoli))
       (result (make-instance 'json-def)))
    (write-json-def-path result `("titolo") "Contenuti")
    (+array-json-def-path result '("sezioni") (length rows-sezioni))
    (loop
      for i from 0 below (length rows-sezioni) do
	(let* 
	    ((id-sezione (get-column (nth i rows-sezioni) "id"))
	     (corsi-sezione (list))
	     (corsi-sezione
	       (progn
		 (mapc
		  (lambda (x)
		    (when (= (get-column x "sezione") id-sezione)
		      (push x corsi-sezione)))
		  rows-corsi)
		 (reverse corsi-sezione))))
	  (+subtree-json-def-path result `("sezioni" ,i))
	  (write-json-def-path result `("sezioni" ,i "id") id-sezione)
	  (write-json-def-path result `("sezioni" ,i "nome") (get-column (nth i rows-sezioni) "nome"))
	  (+array-json-def-path result `("sezioni" ,i "corsi") (length corsi-sezione))
	  (loop
	    with video-corso and articoli-corso
	    for j from 0 below (length corsi-sezione) do
	      (+subtree-json-def-path result `("sezioni" ,i "corsi" ,j))
	      (write-json-def-path result `("sezioni" ,i "corsi" ,j "nome") (get-column (nth j corsi-sezione) "nome"))
	      ;; write videos
	      (setf video-corso (list))
	      (setf video-corso
		    (progn
		      (mapc
		       (lambda (x)
			 (when (equalp (get-column x "nome_corso")
				       (get-column (nth j corsi-sezione) "nome"))
			   (push x video-corso)))
		       rows-video)
		      (reverse video-corso)))
	      (+array-json-def-path result `("sezioni" ,i "corsi" ,j "video") (length video-corso))
	      (loop for k from 0 below (length video-corso) do
		(+subtree-json-def-path result `("sezioni" ,i "corsi" ,j "video" ,k ))
		(write-json-def-path result `("sezioni" ,i "corsi" ,j "video" ,k "nome") (get-column (nth k video-corso) "nome"))
		(write-json-def-path result `("sezioni" ,i "corsi" ,j "video" ,k "link") (get-column (nth k video-corso) "link"))
		(write-json-def-path result `("sezioni" ,i "corsi" ,j "video" ,k "pubblico") (get-column (nth k video-corso) "pubblico"))		
		    )
	      ;; write articles
	      (setf articoli-corso (list))
	      (setf articoli-corso
		    (progn
		      (mapc
		       (lambda (x)
			 (when (equalp (get-column x "nome_corso")
				       (get-column (nth j corsi-sezione) "nome"))
			   (push x articoli-corso)))
		       rows-articoli)
		      (reverse articoli-corso)))
	      (+array-json-def-path result `("sezioni" ,i "corsi" ,j "articoli") (length articoli-corso))
	      (loop for k from 0 below (length articoli-corso) do
		(+subtree-json-def-path result `("sezioni" ,i "corsi" ,j "articoli" ,k ))
		(write-json-def-path result `("sezioni" ,i "corsi" ,j "articoli" ,k "nome") (get-column (nth k articoli-corso) "nome"))
		(write-json-def-path result `("sezioni" ,i "corsi" ,j "articoli" ,k "descrizione") (get-column (nth k articoli-corso) "descrizione"))
		(write-json-def-path result `("sezioni" ,i "corsi" ,j "articoli" ,k "contenuto") (get-column (nth k articoli-corso) "contenuto"))
		(write-json-def-path result `("sezioni" ,i "corsi" ,j "articoli" ,k "pubblico") (get-column (nth k articoli-corso) "pubblico"))		
		    )
	    )))
    result))
	  
				   
    
(defun list-sezioni (&key private)
  (execute-sql
   (format
    nil
    "select * from sezioni ~a order by sequenza;"
    (if private "" "where pubblico=true"))))
	   

(defparameter +restricted-video-code+
  "<div class=\"restricted-content\">Contenuto per abbonati</div>")

(Defun list-video (id-corso &key private)
  (execute-sql
   (format nil
	   "select id,nome,~a sequenza, pubblico from video where corso=~d order by sequenza, nome"
	   (if private
	       "link, "
	       (format nil "case when pubblico=true then link else '~a' end as link, " +restricted-video-code+))
	   id-corso)))

(defun list-corsi (&key video private)
  (let ((result
	  (execute-sql
	   "select * from corsi order by livello, nome"))
	(populated-result (list)))
    (if (and
	   video
	   result
	   (not (zerop (length result))))
	(dolist
	    (corso result populated-result)
	  (let
	      ((video-list
		 (list-video (cdr (assoc "id" corso :test #'equalp)) :private private)))
	    (setf populated-result
		  (append
		   populated-result
		   (list
		    (if video-list
			(append
			 corso
			 `(("video" . ,video-list)))
			corso))))))
	result)))
;;
;; Store new video
;;
(defun store-video(nome link id-corso sequenza pubblico)
  (execute-sql
   (format nil
	   "insert into video (nome, link, corso, sequenza, pubblico)
 values ('~a', '~a', ~d, ~d, ~a) returning id;"
	   nome link id-corso sequenza (if pubblico "true" "false"))))


;;
;; Store new article
;;
(defun store-articolo(nome contenuto id-corso sequenza pubblico)
  (let
      ((sql
	 (format nil
	   "insert into articoli (nome, contenuto, corso, sequenza, pubblico)
 values ('~a', '~a', ~d, ~d, ~a) returning id;"
	   nome
	   (escape-sql contenuto)
	   id-corso
	   sequenza
	   (if pubblico "true" "false"))))
    (hunchentoot:log-message* :warning "~%~%SQL>>> ~a~%~%" sql)
    (execute-sql sql)))

