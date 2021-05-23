(defpackage :radiance-pool
  (:nicknames :radpool)
  (:use :cl  :cl-postgres :fiveam :BORDEAUX-THREADS)
  (:export :define-connector :define-disconnector :define-executor :initialize 
   :pool :reset :force-new-pool :pool-report :pooled-query 
   :test-package)
  (:documentation "
The RADIANCE-POOL package: pooled access to a single database. Designed to be used inside a backend,
it's supposed to never invoke the debugger - to run unattended - and safely handles multi-threaded calls.
Transactions aren't implemented so far.
In loving memory of Pool of Radiance (https://en.wikipedia.org/wiki/Pool_of_Radiance), one of
my first CRPGs.

Usage:
* general: customizazion
  The POOL object is a singleton. It is created by the first call to any of the following:
  - (MAKE-INSTANCE 'RADIANCE-POOL:POOL) => create or access the singleton
  - (RADIANCE-POOL:POOL-REPORT) => print information about the singleton; implicitly creates one
  - (RADIANCE-POOL:POOLED-QUERY) => returns the result of a query on an implicit pooled connection
  - (RADIANCE:TEST-PACKAGE) => run the test suite, creating/recreating pools as necessary.
  And it is (re)created by
  - (force-new-pool) => dangerous: forcibly closes all pooled connections and recreates the singleton

  Default pool parameters are as follows: local postgresql instance, user 'scratch-owner',
  database 'scratch', password 'none', maximum number of connections: 10.

  Pool parameters must thus be customized before first use.
  - Step 1 (optional):
   If the backend for your use case is a PostgreSQL server, you can ignore db-connector,
   db-disconnector and db-executor.
   If you use a different database engine - or you want to customize default behaviour - you must
   provide your own hook functions. Best way to do this is with the three macros
   DEFINE-CONNECTOR, DEFINE-DISCONNECTOR and DEFINE-EXECUTOR: see below how they are used to generate
   default PostgreSQL hooks. 
  - Step 2:
  call
  (RADIANCE:INITIALIZE 
   (&KEY 
    (DB-NAME 'scratch') ; => database for pooled connections
    (DB-USER 'scratch-owner')(db-password 'none') ; => credentials for pooled connections
    (DB-HOST 'localhost') (db-port 5432) ; => the database host and port for pooled connections
    (DB-CONNECTOR #'postgresql-connector) ; => connection hook
    (DB-DISCONNECTOR #'postgresql-disconnector) ; => disconnection hook
    (DB-EXECUTOR #'postgresql-executor) ; => query execution hook
    (MAX-CONNECTIONS *default-max-connections*) ; => maximum number of pooled connections
    (TEST-QUERY *test-query*)) ; => text of query to be used in test suite

* case 1: tests
  Customize your POOL as described above, then (RADIANCE:TEST-PACKAGE). 
* case 2: production
  Customize your POOL as described above, then (RADIANCE:POOLED-QUERY <query-text>) wherever you need
  to read from or write to your database. Successful operations will return the query result in the format
  defined in your :DB-EXECUTOR (list of rows as alists is the default).
  If a condition is signaled along the chain of implicit connect-execute-disconnect operations,
  POOLED-QUERY will return (VALUES NIL CONDITION STACK-TRACE)
"))
