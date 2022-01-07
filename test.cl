;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2017
;;; Last Modified  <michael 2022-01-07 17:13:07>

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

(defun test-nof (n)
  (setf *max-log-file-bytes* 10000)
  (loop
    :for k :below n
    :do (log2:info "Log message ~a written at local time ~a" k (now))))


(defun test-threads (n)
  (let ((*max-log-file-bytes* 10000))
    (setf (log-destination "LOG2") "log2.log")
    (flet ((log-it ()
             (loop
               :for k :below n
               :do (progn
                     (log2:info "Log message ~a written at local time ~a" k (now))
                     (now) (now) (loop for k below 1000 collect k)
                     (log2:info "Log message ~a written at local time ~a" k (now))))))
      (mapcar (lambda (name)
                (bordeaux-threads:make-thread (function log-it) :name name))
              (loop :for k :below n :collect (format nil "THREAD-~a" k))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
