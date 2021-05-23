(defsystem "radiance-pool"
  :description "A concurrency-safe single-database connection pool"
  :author "Salvatore Uras yogavidya@gmail.com"
  :license  "MIT"
  :version "1.0"
  :depends-on ("usocket" "closer-mop" "cl-postgres" "fiveam" "bordeaux-threads")
  :components ((:file "package")
	       (:file "connection-data")
	       (:file "pool")
	       (:file "tests")))
