;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Simple logging module
;;; Created        29/06/2003 00:13:40
;;; Last Modified  <michael 2019-01-31 23:42:28>

(declaim (optimize speed (safety 1) (debug 0)))

(in-package "LOG2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Todo:

(defparameter +level-names+ (make-array 6
                                        :element-type 'string
                                        :initial-contents #("FATAL" "ERROR" "WARNING" "INFO" "DEBUG" "TRACE")))
(declaim (type (simple-array string 1) +level-names+))

(defparameter +fatal+ 0)
(defparameter +error+ 1)
(defparameter +warning+ 2)
(defparameter +info+ 3)
(defparameter +debug+ 4)
(defparameter +trace+ 5)

(defparameter +prefix-format+ "~a [~7@a] <~a> ~{~a~^:~}~,8T")

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
  (log-level% (reverse (cl-utilities:split-sequence #\: category))))

(defun log-level% (category)
  (loop
     :for cat :on category
     :for level = (gethash cat *log-levels*)
     :when level :do (return level) 
     :finally (return *default-log-level*)))

(defun set-log-level (category level)
  (setf (gethash (reverse (cl-utilities:split-sequence #\: category)) *log-levels*) level))

(defsetf log-level set-log-level)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Log streams by 'category' (package and function)

(defparameter *default-log-stream* *standard-output*)

(defparameter *log-streams* (make-hash-table :test #'equalp))

(defun log-stream (category)
  (log-stream% (reverse (cl-utilities:split-sequence #\: category))))
  
(defun log-stream% (category)
  (loop
     :for cat :on category
     :for level = (gethash cat *log-streams*)
     :when level :do (return level) 
     :finally (return *default-log-stream*)))

(defun set-log-stream (category stream)
  (setf (gethash (reverse (cl-utilities:split-sequence #\: category)) *log-streams*) stream))
(defsetf log-stream set-log-stream)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging messages

(defun current-thread-name ()
  (bordeaux-threads:thread-name (bordeaux-threads:current-thread)))

(defun message (level category formatter &rest args)
  (declare (fixnum level)
           (ftype (function (t) fixnum) log-level%))
  (when (and *logging*
             (<= level
                 (log-level% category)))
    (let ((timestamp
           (format-timestring nil (now) :format *timestamp-format* :timezone +utc-zone+))
          (stream (log-stream% category)))
      (multiple-value-bind (result error)
          (ignore-errors
            (bordeaux-threads:with-lock-held ((get-stream-lock stream))
              (apply #'format stream formatter timestamp
                     (aref +level-names+ level)
                     (current-thread-name)
                     (reverse category)
                     args)
              (force-output stream))
            (values t nil))
        (if error
            (warn "Error ~a occurred during logging" error)
            result)))))

(defparameter *stream-locks* (make-hash-table :test #'eq))

(defun get-stream-lock (stream)
  (or (gethash stream *stream-locks* nil)
      (setf (gethash stream *stream-locks*)
            (bordeaux-threads:make-lock))))

(defmacro fatal (format &rest arguments &environment env)
  (let* ((fmt (concatenate 'string +prefix-format+ format "~&"))
         (blockname (enclosing-scope-block-name nil env))
         (category (cons (package-name *package*) blockname)))
    `(message +fatal+ ',(reverse category) ,fmt ,@arguments)))
(defmacro error (format &rest arguments &environment env)
  (let* ((fmt (concatenate 'string +prefix-format+ format "~&"))
         (blockname (enclosing-scope-block-name nil env))
         (category (cons (package-name *package*) blockname)))
    `(message +error+ ',(reverse category) ,fmt ,@arguments)))
(defmacro warning (format &rest arguments &environment env)
  (let* ((fmt (concatenate 'string +prefix-format+ format "~&"))
         (blockname (enclosing-scope-block-name nil env))
         (category (cons (package-name *package*) blockname)))
    `(message +wanring+ ',(reverse category) ,fmt ,@arguments)))
(defmacro info (format &rest arguments &environment env)
  (let* ((fmt (concatenate 'string +prefix-format+ format "~&"))
         (blockname (enclosing-scope-block-name nil env))
         (category (cons (package-name *package*) blockname)))
    `(message +info+ ',(reverse category) ,fmt ,@arguments)))
(defmacro debug (format &rest arguments &environment env)
  (let* ((fmt (concatenate 'string +prefix-format+ format "~&"))
         (blockname (enclosing-scope-block-name nil env))
         (category (cons (package-name *package*) blockname)))
    `(message +trace+ ',(reverse category) ,fmt ,@arguments)))
(defmacro trace (format &rest arguments &environment env)
  (let* ((fmt (concatenate 'string +prefix-format+ format "~&"))
         (blockname (enclosing-scope-block-name nil env))
         (category (cons (package-name *package*) blockname)))
    `(message +trace+ ',(reverse category) ,fmt ,@arguments)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

