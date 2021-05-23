#-asdf3.1 (error "lisper backend requires ASDF 3.1")
(asdf:defsystem #:lisper-backend
  :description "Backend for lisper.ch website"
  :author "Salvatore Uras yogavidya@gmail.com"
  :license  "MIT"
  :version "0.0.1"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on 
  ("swank"
   "lisp-unit"
   "cl-ppcre"
   "cl-interpol"
   "alexandria"
   "closer-mop"
   "hunchentoot"
   "cl-json"
   "split-sequence"
   "radiance-pool"
   "cl-smtp")
  
  :components 
  ((:file "package")
   (:file "storage")
   (:file "server")))

