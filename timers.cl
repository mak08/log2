;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2018
;;; Last Modified  <michael 2018-07-11 23:28:48>

(defpackage "TIMERS"
  (:use "COMMON-LISP" "LOCAL-TIME")
  (:EXPORT "TIMERS-RUNNING-P"
           "REMOVE-ALL-TIMERS"
           "ADD-TIMER"
           "REMOVE-TIMER"
           "START-TIMERS"
           "STOP-TIMERS"))

(in-package :timers)

(defstruct timer id function spec bindings)

(defvar *timers* nil)
(defvar *timer-count* 0)
(defvar *timer-running* nil)

(defun timers-running-p ()
  *timer-running*)

(defun remove-all-timers ()
  (setf *timers* nil))

(defun add-timer (function &key
                             (id (format nil "TIMER-~a" (incf *timer-count*)))
                             (hours nil)
                             (minutes nil)
                             (bindings nil))
  (when (find id *timers* :key #'timer-id :test #'string=)
    (error "Timer ~a already exists" id))
  (push (make-timer :id id
                    :function function
                    :spec (list :hours hours :minutes minutes)
                    :bindings bindings)
        *timers*)
  (values id))

(defun remove-timer (id)
  (setf *timers*
        (remove id *timers* :key #'timer-id :test #'string=)))

(defun spec-matches-p (timestamp spec)
  (let ((minute (timestamp-minute timestamp :timezone +utc-zone+))
        (hour (timestamp-hour timestamp :timezone +utc-zone+)))
    (flet ((matches (number spec)
             (or (null spec)
                 (member number spec))))
      (and (matches hour (getf spec :hours))
           (matches minute (getf spec :minutes))))))

(defun timer-loop ()
  (sleep (- 60 (timestamp-second (now))))
  (do ()
      ((not *timer-running*)
       t)
    (log2:info "Checking timers" )
    (let ((now (now)))
      (dolist (timer *timers*)
        (when (spec-matches-p now (timer-spec timer))
          (let ((bordeaux-threads:*default-special-bindings* (timer-bindings timer)))
            (bordeaux-threads:make-thread (timer-function timer) :name (timer-id timer)))))
      (sleep (- 60 (timestamp-second (now)))))))

(defun start-timers ()
  (when *timer-running*
    (error "Timers already running"))
  (setf *timer-running* t)
  (bordeaux-threads:make-thread #'timer-loop :name "TIMER-LOOP"))

(defun stop-timers ()
  (setf *timer-running* nil))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
