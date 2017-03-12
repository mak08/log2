;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Simple logging module
;;; Created        29/06/2003 00:13:40
;;; Last Modified  <michael 2017-03-12 18:02:38>

(in-package "LOG2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Todo:

(defparameter +level-names+ #("FATAL" "ERROR" "WARNING" "INFO" "DEBUG" "TRACE"))

(defparameter +fatal+ 0)
(defparameter +error+ 1)
(defparameter +warning+ 2)
(defparameter +info+ 3)
(defparameter +debug+ 4)
(defparameter +trace+ 5)

(defparameter +prefix-format+ "~a [~a] ~{~a~^:~}~T")

(defparameter *logging* t)

(defparameter *timestamp-format* '((:year 4) #\- (:month 2) #\- (:day 2) #\space
                                   (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:msec 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Log levels by package and function
;;;
;;; Each logging statement is associated with the package and function where it
;;; appears. Log levels can be defined per package and per function.If a
;;; function has no associated log-level, the log level of the package is used.

(defparameter *default-log-level* +info+)

(defparameter *log-levels* (make-hash-table :test #'equalp))

(defun log-level (category)
  (destructuring-bind (package &optional function &rest args)
      (cl-utilities:split-sequence #\: category)
    (or (and function
             (gethash category *log-levels*))
        (gethash package *log-levels*)
        *default-log-level*)))

(defun set-log-level (category level)
  (setf (gethash category *log-levels*) level))

(defsetf log-level set-log-level)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Log streams by 'category' (package and function)

(defparameter *default-log-stream* *standard-output*)

(defparameter *log-streams* (make-hash-table :test #'equalp))

(defun log-stream (category)
  (destructuring-bind (package &optional function &rest args)
      (cl-utilities:split-sequence #\: category)
    (or (and function
             (gethash category *log-streams*))
        (gethash package *log-streams*)
        *default-log-stream*)))

(defun set-log-stream (category stream)
  (setf (gethash category *log-streams*) stream))

(defsetf log-stream set-log-stream)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging messages

(defun message (level category formatter &rest args)
  (let ((category-name (format () "~@:(~a~):~@:(~a~)" (car category) (cadr category))))
    (when (and *logging*
               (<= level (log-level category-name)))
      (let ((timestamp
             (format-timestring nil (now) :format *timestamp-format* :timezone +utc-zone+))
            (stream (log-stream category-name)))
        (multiple-value-bind (result error)
            (ignore-errors
              (bordeaux-threads:with-lock-held ((get-stream-lock stream))
                (apply #'format stream formatter timestamp (aref +level-names+ level) category args)
                (force-output stream))
              (values t nil))
          (if error
              (warn "Error ~a occurred during logging" error)
              result))))))

(defparameter *stream-locks* (make-hash-table :test #'eq))

(defun get-stream-lock (stream)
  (or (gethash stream *stream-locks* nil)
      (setf (gethash stream *stream-locks*)
            (bordeaux-threads:make-lock))))

(eval-when (:load-toplevel :compile-toplevel :execute)
(defmacro define-log-macro (name level)
  `(defmacro ,name (format &rest args &environment env)
     (let ((fmt
            (concatenate 'string +prefix-format+ format "~&"))
           (blockname (enclosing-scope-block-name nil env)))
       `(let ((category (cons (package-name ,*package*) ',blockname)))
          (apply #'message ,,level category ,`(formatter ,fmt) (list ,@args))))))
)

(define-log-macro fatal +fatal+)
(define-log-macro error +error+)
(define-log-macro warning +warning+)
(define-log-macro info +info+)
(define-log-macro debug +debug+)
(define-log-macro trace +trace+)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

