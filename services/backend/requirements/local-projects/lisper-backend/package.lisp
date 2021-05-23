(defpackage :lisper-backend
  (:nicknames :lsb)
  (:use :cl :alexandria :cl-interpol :cl-ppcre :hunchentoot)
  (:export
   :start-server :stop-server))

  (shadowing-import '(:standard-method :standard-class :defmethod :defgeneric :standard-generic-function) :closer-mop)

