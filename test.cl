;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2017
;;; Last Modified  <D037165 2019-02-01 10:39:57>

(defpackage "LOG2-TEST"
  (:use "COMMON-LISP"))

(in-package :log2-test)

(defun test ()
  (let ((x (random 10)))
    (flet ((inner-1 ()
             (flet ((inner-2 ()
                      (log2:info "x=~a" (* x 4))))
               (inner-2))))
      (inner-1))))

(defun ptest (n)
  (dotimes (k n)
    (test)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        


