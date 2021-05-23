(in-package :radiance-pool)

;;
;; CLASS: connection-data
;;
(defmethod get-stack-trace ((condition condition))
  (with-output-to-string (out-string) 
    (uiop/image:print-condition-backtrace condition :stream out-string) 
    out-string))

(defmacro define-connector ((name connection-data-var) &body connection-forms)
"Creates a DB-CONNECTOR hook for (INITIALIZE).
IN: NAME: hook's name
    CONNECTION-DATA-VAR: the variable for CONNECTION-DATA
    CONNECTION-FORMS: implicit PROGN using data in CONNECTION-DATA-VAR to create
      and return a connection
OUT: a function named NAME to be used as a :DB-CONNECTOR argument for (INITIALIZE)"
  `(defun ,name (,connection-data-var)
     (handler-case
         (progn ,@connection-forms)
       (condition (condition) (values nil condition (get-stack-trace condition))))))

(defmacro define-disconnector ((name connection-var) &body disconnection-forms)
"Creates a DB-DISCONNECTOR hook for (INITIALIZE).
IN: NAME: hook's name
    CONNECTION-VAR: the connection variable
    DISCONNECTION-FORMS: implicit PROGN to disconnect CONNECTION-VAR
OUT: a function named NAME to be used as a :DB-DISCONNECTOR argument for (INITIALIZE)"  
`(defun ,name (,connection-var)
     (handler-case
         (progn ,@disconnection-forms)
       (condition (condition) (values nil condition (get-stack-trace condition)))
       (:no-error () t))))


(defmacro define-executor ((name connection-var query-var error-restarts) &body execute-forms)
"Creates a DB-EXECUTOR hook for (INITIALIZE).
IN: NAME: hook's name
    CONNECTION-VAR: the connection variable
    QUERY-VAR: the query string variable
    ERROR-RESTARTS: a list of condition handlers in the format used by HANDLER-CASE,
      i.e. (CONDITION-TYPE (CONDITION-VAR) FORMS*)
      Note: you can trigger a re-execution of the query by invoking (executor) inside
      a condition handler. Be sure not to be stuck in a closed loop.
    EXECUTE-FORMS: implicit PROGN that actually executes QUERY-VAR and returns rows
OUT: a function named NAME to be used as a :DB-EXECUTOR argument for (INITIALIZE)"  
`(defun ,name (,connection-var ,query-var)
   (labels 
       ((executor ()
          (handler-case
              (progn ,@execute-forms)
            ,@error-restarts
                (error (condition) condition))))
         (let ((result (executor)))
           (disconnect (connection-data (make-instance 'pool)) ,connection-var)
           (if (typep result 'condition)
               (progn 
                 (values nil result (get-stack-trace result)))
             result)))))
;;
;; Default CONNECTION-DATA hooks for PostgreSQL
;;
(define-connector (postgresql-connector conn-data)
                  (cl-postgres:open-database (name conn-data)
                                             (user conn-data)
                                             (password conn-data)
                                             (host conn-data)
                                             (port conn-data)))

(define-disconnector (postgresql-disconnector conn)
      (cl-postgres:close-database conn))

(define-executor (postgresql-executor conn 
                                      query-txt 
                                      ((database-connection-lost () (cl-postgres:reopen-database conn)
                                                                 (executor))))
  (exec-query conn query-txt 'alist-row-reader))
;;
;; Class: CONNECTION-DATA
;;
(defclass connection-data ()
  ((name :type string 
         :initarg :name 
         :reader name
         :documentation "Database name")
   (host :type string 
         :initarg :host 
         :initform "localhost" 
         :reader host
         :documentation "Host of the database server")
   (port :type fixnum 
         :initarg :port 
         :initform 5432 
         :reader port
         :documentation "TCP port of the database server")
   (user :type string 
         :initarg :user 
         :reader user
         :documentation "Username for connections")
   (password :type string 
             :initarg :password 
             :reader password
             :documentation "Password for connections")
   (connect-fn :type function 
               :initarg :connect-fn 
               :initform #'postgresql-connector 
               :reader connect-fn
               :documentation "Connect hook") 
   (disconnect-fn :type function 
                  :initarg :disconnect-fn 
                  :initform #'postgresql-disconnector 
                  :reader disconnect-fn
                  :documentation "Disconnect hook")
   (execute-query-fn :type function 
                     :initarg :execute-query-fn 
                     :initform #'postgresql-executor 
                     :reader execute-query-fn
                     :documentation "Query execute hook")))


(defmethod connect ((this connection-data))
  "Implicitly called by POOL"
  (funcall (connect-fn this) this))

(defmethod disconnect ((this connection-data) conn)
  "Implicitly called by POOL"
  (funcall (disconnect-fn this) conn))

(defmethod execute-query ((this connection-data)  conn (query string))
  "Implicitly called by POOL"
  (funcall (execute-query-fn this) conn query))

(defparameter *default-connection-data* 
  (make-instance 'connection-data 
                 :name "scratch"
                 :user "scratch-owner"
                 :password "none")
  "Set by INITIALIZE, used by POOL")

(defun initialize  
       (&key (db-name "scratch") (db-user "scratch-owner") 
             (db-password "none") (db-host "localhost") (db-port 5432)
             (db-connector #'postgresql-connector)
             (db-disconnector #'postgresql-disconnector)
             (db-executor #'postgresql-executor)
             (max-connections *default-max-connections*)
             (test-query *test-query*))
       "Builds CONNECTION-DATA implicitly used by POOL methods.
        IN: DB-NAME, DB-HOST, DB-PORT: database coordinates
            DB-USER, DB-PASSWORD: database credentials
            DB-CONNECTOR, DB-DISCONNECTOR, DB-EXECUTOR:
              hooks created by DEFINE-* macros and
              implicitly used by CONNECTION-DATA and POOL
            MAX-CONNECTIONS: maximum number of pooled connections
              handled by POOL. Operations requiring more than
              MAX-CONNECTIONS will return (NIL POOL-OVERFLOW)"
  (setf *default-connection-data* 
        (make-instance 'connection-data
                       :name db-name :host db-host
                       :port db-port :user db-user
                       :password db-password
                       :connect-fn db-connector
                       :disconnect-fn db-disconnector
                       :execute-query-fn db-executor))
  (force-new-pool :max-connections max-connections)
  (setf *test-query* test-query)
  T)
