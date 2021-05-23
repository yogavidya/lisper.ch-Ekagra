(in-package :lisper-backend)

;;;----------------------------------------------------------
;;; Acceptor specifications
;;;----------------------------------------------------------

(define-symbol-macro +www-root+ (merge-pathnames
				 "lisper.ch/lisper-site/dist/"
				 (user-homedir-pathname)))

(defclass vhost (hunchentoot:acceptor)
  ((server-instance
    :accessor server-instance
    :allocation :class
    :initform nil
    :documentation
    "The one server instance, read/write with [get/set]-server-instance"))
  (:documentation
  "The lisper.ch hunchentoot acceptor subclass."))

(defun get-server-instance()
  (server-instance
   (closer-mop:class-prototype (find-class 'vhost))))

(defun set-server-instance(create-instance-fn)
  (setf
   (server-instance
    (closer-mop:class-prototype (find-class 'vhost)))
   (when create-instance-fn
     (funcall create-instance-fn))))
  
(defun make-server-instance ()
  (make-instance 'vhost
		 :address "0.0.0.0"
		 :port 9999
		 :document-root +www-root+))

(defmethod handle-request :around ((vhost vhost) request)
  (log-message* :info "script-name is: ~s~%" (slot-value request 'script-name))
  (let 
    ((acceptable-origin 
      (or
       (and (>= (length (header-in* :referer)) 24)
            (equalp 
             (subseq (header-in* :referer) 0 24)
             "https://lisper.ch/Ekagra"))
        (equalp 
         (header-in* :origin) 
         "http://localhost:9000"))))
    (if (null acceptable-origin)
      (progn
        (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
        (cl-json:encode-json-alist-to-string
         '((result . "Accesso proibito"))))
      (cond
       ;;
       ;; PREFLIGHT
       ;;
       ((eq (request-method request) :OPTIONS)
        (fn-api-preflight))
       ;;
       ;; TEST ROUTES
       ;;
       ((and (eq (request-method request) :GET)
             (string-equal (slot-value request 'script-name) "/Ekagra-api/test"))
        (fn-api-test))
       ((and (eq (request-method request) :POST)
             (string-equal (slot-value request 'script-name) "/Ekagra-api/test"))
        (fn-api-test-post))
       ;;
       ;; AUTHORIZATION ROUTES
       ;;
       ((and (eq (request-method request) :POST)
             (string-equal (slot-value request 'script-name) "/Ekagra-api/authorize"))
        (fn-api-authorize))    
       ((and (eq (request-method request) :POST)
             (string-equal (slot-value request 'script-name) "/Ekagra-api/close-session"))
        (fn-api-close-session))
       ((and (eq (request-method request) :POST)
             (string-equal (slot-value request 'script-name) "/Ekagra-api/register"))
        (fn-api-register))
       ((and (eq (request-method request) :POST)
             (string-equal (slot-value request 'script-name) "/Ekagra-api/confirm"))
        (fn-api-confirm))
       ((and (eq (request-method request) :POST)
             (string-equal (slot-value request 'script-name) "/Ekagra-api/extend-session"))
        (fn-api-extend-session))
       ;;
       ;; PAYPAL NOTIFICATION ROUTES
       ;;
       ((and T ;(eq (request-method request) :POST)
             (string-equal (slot-value request 'script-name) "/Ekagra-api/paypal-notification"))
        (fn-api-paypal-notification-handler))
       ;;
       ;; CONTENT MENU DATA
       ;;
       ((and (eq (request-method request) :GET)
             (string-equal (slot-value request 'script-name) "/Ekagra-api/content-menu-data"))
        (fn-api-content-menu-data))        
       ;;
       ;; RESOURCE: CORSI
       ;;
       ((and (eq (request-method request) :GET)
             (string-equal (slot-value request 'script-name) "/Ekagra-api/lista-corsi"))
        (fn-api-lista-corsi))        
       ;;
       ;; RESOURCE: VIDEO
       ;;
       ((and (eq (request-method request) :POST)
             (string-equal (slot-value request 'script-name) "/Ekagra-api/store-new-video"))
        (fn-api-store-video))
       ;;
       ;; RESOURCE: ARTICLES
       ;;
       ((and (eq (request-method request) :POST)
             (string-equal (slot-value request 'script-name) "/Ekagra-api/store-new-article"))
        (fn-api-store-article))
       ;;
       ;; FALLBACK TO DEFAULT
       ;;
       (T
        (call-next-method))))))

(defun encode-json-result-message (payload)
  (cl-json:encode-json-alist-to-string `((:|result| . ,payload))))


;;;----------------------------------------------------------
;;; Entry points
;;;----------------------------------------------------------
(defun stop-server ()
  (let
      ((server-instance (get-server-instance)))
    (when server-instance
	(block stop-if-running-and-clear
	  (when (hunchentoot:started-p server-instance)
		(hunchentoot:stop server-instance :soft t))
	  (set-server-instance nil)))))

(defun start-server ()
  (stop-server)
  (set-server-instance #'make-server-instance))


;;;----------------------------------------------------------
;;; HTTP request handlers
;;;----------------------------------------------------------

;; General utilities

(defun write-default-headers ()
  #+EKAGRA-PRODUCTION (setf (hunchentoot:header-out :Access-Control-Allow-Origin) "https://lisper.ch")
  #-EKAGRA-PRODUCTION (setf (hunchentoot:header-out :Access-Control-Allow-Origin) "http://localhost:9000")
  (setf (hunchentoot:header-out :Vary) "Origin")
  (setf (hunchentoot:header-out :Content-Type) "application/json")
  (setf (hunchentoot:header-out :Access-Control-Allow-Headers)
	"lisperClient,content-type,Authorization"))
  
(defun json-payload ()
  (cl-json:decode-json-from-string (raw-post-data :force-text T)))

(defun json-parameter (p json-alist)
  (cdr (assoc p json-alist :test #'equalp)))

(defun fn-api-preflight ()
  (write-default-headers)
  (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
  "OK")

;; Paypal IPN utilities

(defun paypal-ipn-querystring()
  (hunchentoot:raw-post-data :force-text T))

(defun paypal-ipn-parameters()
  (let ((querystring  (paypal-ipn-querystring))
        (result (list)))
    (cl-ppcre:do-matches-as-strings
        (i "(((\\w*)[\\=]([\\w]*[\\.]?[\\w]*[\\%40]?[\\w]*[\\.]?[\\w]*))[\\&]?)" querystring)
      (multiple-value-bind
          (ret match-array)
          (cl-ppcre:scan-to-strings "(\\w*)[\\=]([\\w]*[\\.]?[\\w]*[\\%40]?[\\w]*[\\.]?[\\w]*)[\\&]?" i)
        (push `(,(aref match-array 0) . ,(aref match-array 1)) result)))
    result))

(defun paypal-ipn-parameter(name parameters-alist)
  (cdr (assoc name parameters-alist :test #'equalp)))


(defun unescape-email(address)
  (let*
      ((pos0 (position #\% address))
       (original-length (length address))
       (pos (and pos0 (> original-length (+ pos0 2))))
       (pos (and pos0 (char-equal #\4 (aref address (+ 1 pos0)))))
       (pos (and pos0 (char-equal #\0 (aref address (+ 2 pos0))))))
    (when pos
      (concatenate 'string
                   (make-array
		    `(,pos0)
		    :element-type (array-element-type address)
		    :displaced-to address)
                   "@"
                   (make-array
		    `(,(- original-length pos0 3))
		    :element-type (array-element-type address)
		    :displaced-to address
		    :displaced-index-offset (+ pos0 3))))))
                   

;; Actual handlers start here
    
; GET /Ekagra-api/test
(defun fn-api-test ()
  (write-default-headers)
  (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
  (cl-json:encode-json-alist-to-string
   '(("result" . "ok")
     ("details" . "test API call successful")
     ("questions" . "why the hell doesn't it send JSON?"))))


; POST /Ekagra-api/test
(defun fn-api-test-post ()
  (let ((parameters (json-payload)))
    (write-default-headers)
    (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
    (cl-json:encode-json-alist-to-string
     `(("result" . "ok")
       ("details" . "test API call successful - POST")
       ("in" . ,(json-parameter :pippo parameters))))))



;; POST /Ekagra-api/authorize
(defun fn-api-authorize()
  (let*
      ((payload (json-payload))
       (payload (json-parameter :data payload))
       (email (json-parameter :email payload))
       (password (json-parameter :password payload))
       (user-id (id-for-credentials email password))
       (token nil)
       (open-sessions (has-open-sessions user-id)))
    (log-message* :info "login request, email: ~s, password: ~s, id: ~a~%"
		  email password user-id)
    (write-default-headers)
    (if (and user-id (not open-sessions)) 
	(progn
	  (defparameter *r* *request*)
	  (setf token (create-session user-id (remote-addr*)))
	  (log-message* :info "token: ~a~%" token)
	  (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
	  (cl-json:encode-json-alist-to-string
	   `((result . "ok")
	     (nickname . ,(nickname-for-id user-id))
	     (token . ,token)
	     (duration-minutes . ,(get-session-duration-minutes)))))
	(progn
	  (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
	  (cl-json:encode-json-alist-to-string
	   '((result . "Credenziali sconosciute")))))))

;; POST /Ekagra-api/close/session
(defun fn-api-close-session()
  (let*
      ((token (header-in* :authorization))
       (token (when token (subseq token 7)))
       (result (close-session token)))
    (log-message* :info "close session for token ~s, result: ~a~%"
		  token result)
    (write-default-headers)
    (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
    (cl-json:encode-json-alist-to-string result)))

;; POST /Ekagra-api/register
(defun fn-api-register()
  (let*
      ((payload (json-payload))
       (payload (json-parameter :data payload))
       (email (json-parameter :email payload))
       (nickname (json-parameter :nickname payload))
       (password (json-parameter :password payload)))
    (log-message*
     :info
     "registration request, email: ~s, nickname ~s, password: ~s~%"
     email nickname password)
    (write-default-headers)
    (if (register-user email nickname password)
	(progn
	  (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
	  (cl-json:encode-json-alist-to-string
	   `((result . "ok"))))
	(progn
	  (setf (hunchentoot:return-code*)
		hunchentoot:+http-bad-request+)
	  (cl-json:encode-json-alist-to-string
	   `((result . "failed")))))))

;; POST /Ekagra-api/confirm
(defun fn-api-confirm()
  (let*
      ((payload (json-payload))
       (id (json-parameter :pending payload)))
    (log-message*
     :info
     "confirmation request, pending=~d~%"
     id)
    (write-default-headers)
    (if (confirm-user id)
	(progn
	  (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
	  (cl-json:encode-json-alist-to-string
	   `((result . "ok"))))
	(progn
	  (setf (hunchentoot:return-code*)
		hunchentoot:+http-bad-request+)
	  (cl-json:encode-json-alist-to-string
	   `((result . "failed")))))))

;; POST /Ekagra-api/extend-session
(defun fn-api-extend-session()
  (let*
      ((token (header-in* :authorization))
       (token (when token (subseq token 7)))
       (result (extend-session token)))
    (log-message* :info "extend session for token ~s, result: ~a~%"
		  token result)
    (write-default-headers)
    (if result
	(setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
	(setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+))
    (cl-json:encode-json-alist-to-string
     `((result . ,(if result "ok" "failed"))))))

;; PAYPAL payment notification
(defun fn-api-paypal-notification-handler()
  (let* ((ipn-parameters (paypal-ipn-parameters))
	 (transaction-type (paypal-ipn-parameter "txn_type" ipn-parameters))
	 (payer-email (lsb::unescape-email (lsb::paypal-ipn-parameter "payer_email" ipn-parameters))))
    (log-message* :info "PAYPAL IPN => customer: ~s, transaction type: ~s~%"
		  payer-email
		  transaction-type)
    (cond
	((equalp transaction-type "subscr_signup")
	 (enable-subscriber payer-email))
	((equalp transaction-type "subscr_cancel")
	 (disable-subscriber payer-email)))
    (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
    nil))


;;
;; PROTECTED or MIXED APIs
;;

(defmacro protected-api(name &body api-forms)
  `(defun ,name ()
    (let*
	((token (header-in* :authorization))
	 (token (when token (subseq token 7)))
	 (valid-session (valid-session-p token))
	 (result (and valid-session (progn ,@api-forms))))
      (write-default-headers)
      (if result
	  (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
	  (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+))
      (cl-json:encode-json-to-string
       result))))


(defmacro mixed-api(name &key protected public)
  `(defun ,name ()
    (let*
	((token (header-in* :authorization))
	 (token (when token (subseq token 7)))
	 (valid-session (valid-session-p token))
	 (result
	   (if valid-session
	       ,protected
	       ,public)))
      (write-default-headers)
      (if result
	  (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
	  (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+))
      (handler-case 
	  (cl-json:encode-json-to-string result)
	(condition (json-condition)
	  (progn
	    (log-message* :error
			  "Failed JSON encoding of payload: ~w~%, condition: ~a~%"
			  result json-condition)
	    (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
	  ""))))))


;; Lista corsi
(protected-api fn-api-lista-corsi
  (list-corsi))

;; Content menu data
(mixed-api fn-api-content-menu-data
	   :public
	   (let* ((all (list-content)))
	     (write-default-headers)
	     (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
	     (cl-json:encode-json-to-string all))
	   :protected
	   (let* ((all (list-content :private T)))
	     (write-default-headers)
	     (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
	     (cl-json:encode-json-to-string all)))


;; POST /Ekagra-api/store-new-video
(protected-api fn-api-store-video
  (let*
      ((data (json-payload))
       (nome (json-parameter :nome data))
       (link (json-parameter :link data))
       (corso (json-parameter :corso data))
       (sequenza (json-parameter :sequenza data))
       (pubblico (json-parameter :pubblico data)))
    (if
     (store-video nome link corso sequenza pubblico)
     (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
     (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+))
    "OK"))


;; POST /Ekagra-api/store-new-article
(protected-api fn-api-store-article
  (let*
      ((data (json-payload))
       (nome (json-parameter :nome data))
       (contenuto (json-parameter :contenuto data))
       (corso (json-parameter :corso data))
       (sequenza (json-parameter :sequenza data))
       (pubblico (json-parameter :pubblico data)))
    (if
     (store-articolo nome contenuto corso sequenza pubblico)
     (setf (hunchentoot:return-code*) hunchentoot:+http-ok+)
     (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+))
    "OK"))


