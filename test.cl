;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2017
;;; Last Modified  <D037165 2019-02-01 13:26:01>

(defpackage "LOG2-TEST"
  (:use "COMMON-LISP"))

(in-package :log2-test)

(defun test ()
  (block test-1
    (block test-2
      (log2:info "x=~a" 44))))

(defun ptest (n)
  (dotimes (k n)
    (test)))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        


