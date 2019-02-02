;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2017
;;; Last Modified  <michael 2019-02-02 20:00:40>

(defpackage "LOG2-TEST-1"
  (:use "COMMON-LISP"))
(in-package :log2-test-1)
(defun write-log-entries (n)
  (block test-1
    (dotimes (k n)
      (sleep 0.01)
      (log-it k))))
(defun log-it (k)
  (log2:info "x=~a" k))


(defpackage "LOG2-TEST-2"
  (:use "COMMON-LISP"))
(in-package :log2-test-2)
(defun write-log-entries (n)
  (block test-2
    (dotimes (k n)
      (sleep 0.01)
      (log-it k))))
(defun log-it (k)
  (log2:info "x=~a" k))

(in-package :log2)

(defun test ()
  (let ((ls "logtest.txt"))
    (setf (log2:log-destination "LOG2-TEST-1") ls)
    (setf (log2:log-destination "LOG2-TEST-2") ls)
    (log2-test-1::write-log-entries 300)
    (log2-test-2::write-log-entries 300)
    (log2-test-1::write-log-entries 300)
    (log2-test-2::write-log-entries 300)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        


