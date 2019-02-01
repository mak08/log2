;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2017
;;; Last Modified  <D037165 2019-02-01 12:14:39>

(defpackage "LOG2-TEST"
  (:use "COMMON-LISP"))

(in-package :log2-test)

(defun test ()
  (log2:info "x=~a" 44))

(defun ptest (n)
  (dotimes (k n)
    (test)))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        


