;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Simple timers.
;;; Copyright      (c)  2018
;;; Last Modified  <michael 2018-11-07 20:49:07>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timer loop is running by default (started on load).
;;; Registered timers are checked approximately each minute.
;;; Multiple invocations of a timer may run at the same time (if the timer runs
;;; longer than its timing interval).

(defpackage "TIMERS"
  (:use "COMMON-LISP" "LOCAL-TIME")
  (:EXPORT "TIMERS-RUNNING-P"
           "ADD-TIMER"
           "REMOVE-TIMER"
           "REMOVE-ALL-TIMERS"
           "START-TIMER-LOOP"
           "STOP-TIMER-LOOP"))

(in-package :timers)

(defstruct timer id function spec bindings)

(defvar *timers* nil)
(defvar *timer-count* 0)
(defvar *timer-loop* nil)

(defun timers-running-p ()
  *timer-loop*)

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
      ((not *timer-loop*)
       t)
    (log2:trace "Checking timers" )
    (let ((now (now)))
      (dolist (timer *timers*)
        (when (spec-matches-p now (timer-spec timer))
          (let ((bordeaux-threads:*default-special-bindings* (timer-bindings timer)))
            (bordeaux-threads:make-thread (timer-function timer) :name (timer-id timer)))))
      (sleep (- 60 (timestamp-second (now)))))))

(defun start-timer-loop ()
  (when *timer-loop*
    (error "Timers already running"))
  (log2:info "Starting timers")
  (setf *timer-loop* t)
  (bordeaux-threads:make-thread #'timer-loop :name "TIMER-LOOP"))

(defun stop-timer-loop ()
  (log2:info "Starting timers")
  (setf *timer-loop* nil))

(eval-when (:execute :load-toplevel)
  (ignore-errors
    (start-timer-loop)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
