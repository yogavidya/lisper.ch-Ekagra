(in-package :radiance-pool)

(defparameter *test-query* "select * from test")

;;
;; connection-data tests
;;
(def-test connection-data ()
  (let* ((bad-connection-data
          (make-instance 'connection-data :name "lkajshd" :user "sdsd" :password "dfsdf"))
         (result-0
          (multiple-value-list
           (connect bad-connection-data)))
         (result-1
          (multiple-value-list
           (connect *default-connection-data*)))
         (result-2
          (and (first result-1)
               (multiple-value-list
                (execute-query *default-connection-data* 
                               (first result-1)
                               *test-query*))))
         (result-3
          (and (first result-1)
               (multiple-value-list
                (disconnect *default-connection-data*
                            (first result-1))))))
    (is-false (first result-0))
    (is-true (second result-0))
    (is-true (first result-1))
    (is-false (second result-1))
    (is-true (first result-2))
    (is-false (second result-2))
    (is-true (first result-3))
    (is-false (second result-3))))


;;
;; pool tests
;;

(def-test pool ()
  (let*
      ((result-0 (force-new-pool))
       (result-1 (make-instance 'pool))
       (p (force-new-pool))
       ;create a set of connection results; last one is invalid
       (result-2 (loop for n from 1 to (1+ (max-connections p))
                       collect (multiple-value-bind 
                                   (r c)
                                   (connect p)
                                 (list r c))))
       ;create a set of query results; last one is invalid
       (result-3 (loop for n from 1 to (1+ (max-connections p))
                       collect (multiple-value-bind 
                                   (r c)
                                   (execute-query p (nth (1- n)(used-connections p))
                                                  *test-query*)
                                 (list r c))))
       ;create a set of disconnection results; last one is invalid
       (result-4 (loop for n from 1 to (1+ (max-connections p))
                       collect (multiple-value-bind 
                                   (r c)
                                   (disconnect p (first (used-connections p)))
                                 (list r c)))))
    ; pool singleton
    (is-true (eq result-0 result-1))
    (is-false (eq p result-0))
    ; all but last in result-2 are valid connections
    (is-true (= (length result-2) (1+ (max-connections p))))
    (dolist (connection-result (subseq result-2 0 (1- (max-connections p))))
      (is-true (and (first connection-result) (null (second connection-result)))))
    (is-true (and (null (first (car (last result-2))))
                  (second (car (last result-2)))))
    ; all but last in result-3 are valid query results
    (is-true (= (length result-3) (1+ (max-connections p))))
    (dolist (connection-result (subseq result-3 0 (1- (max-connections p))))
      (is-true (and (first connection-result) (null (second connection-result)))))
    (is-true (and (null (first (car (last result-3))))
                  (second (car (last result-3)))))
    ;all disconnects in result-4 successful  but last
    (is-true (= (length result-4) (1+ (max-connections p))))
    (dolist (connection-result (subseq result-4 0 (1- (max-connections p))))
      (is-true (and (first connection-result) (null (second connection-result)))))
    (is-true (and (null (first (car (last result-4))))
                  (second (car (last result-4)))))
    ;reset successfully empties pool
    (is-false (used-connections p))
    (reset p)
    (is-false (connections p))))

 
(defparameter *concurrent-test-results* nil)
(defparameter *concurrent-test-lock* (make-lock))
(defparameter *concurrent-test-index* 0)
(defparameter *concurrent-tests* 50)
(defparameter *concurrent-test-start* nil)
(defparameter *concurrent-tests-done* 0)

(def-test concurrent-operations ()
  ; ensure pool empty
  (force-new-pool :max-connections 90)
  (loop while (< *concurrent-test-index* *concurrent-tests*) do
    (make-thread 
     (lambda() 
       (loop while (null *concurrent-test-start*) do (sleep (/ (random  1000) 1000)))
       (let ((start-time (get-internal-real-time))
             (conn (connect (make-instance 'pool))))
         (multiple-value-bind (r c)
             (execute-query (make-instance 'pool) conn *test-query*)
           (disconnect (make-instance 'pool) conn)
           (with-lock-held (*concurrent-test-lock*)
             (push  `((thread ,$n) (result ,r) (condition ,c) 
                          (time ,(float (/ (- (get-internal-real-time) start-time) internal-time-units-per-second ))))
                    *concurrent-test-results*))))
       (incf *concurrent-tests-done*))
     :name (format nil "concurrency thread ~D" *concurrent-test-index*) 
     :initial-bindings `(($n . ,*concurrent-test-index*)))
    (incf *concurrent-test-index*))
  (setf *concurrent-test-start* T)
  ;wait until all threads done
  (loop while (< *concurrent-tests-done* *concurrent-tests*) do (sleep .1))
  (acquire-lock *concurrent-test-lock* T)
  (dolist (current-result *concurrent-test-results*)
    (is-true (first (assoc 'result current-result))))
  (is-true (= *concurrent-tests* (length *concurrent-test-results*)))
  (release-lock *concurrent-test-lock*))

(defun test-package ()
  (explain! (run 'connection-data))
  (explain! (run 'pool))
  (explain! (run 'concurrent-operations)))

