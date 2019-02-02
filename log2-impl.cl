;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Simple logging module
;;; Created        29/06/2003 00:13:40
;;; Last Modified  <michael 2019-02-02 20:20:57>

(declaim (optimize speed (safety 1) (debug 0)))

(in-package "LOG2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

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

(defparameter *log-create-policy* :create)
(defparameter *log-overwrite-policy* :append)
(defparameter *max-log-file-bytes* 20000000)

(defparameter *timestamp-format* '((:year 4) #\- (:month 2) #\- (:day 2) #\space
                                   (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:msec 3)))

(defparameter *default-log-level* +info+)
(defparameter *log-level-ht* (make-hash-table :test #'equalp))
(defvar +log-level-table-lock+ (bordeaux-threads:make-lock))

(defparameter *default-log-destination* *standard-output*)
(defparameter *log-destination-ht* (make-hash-table :test #'equalp)
  "Category -> Destination")
(defparameter *log-stream-ht* (make-hash-table :test #'equalp)
  "Destination -> Stream")
(defparameter +log-destination-table-lock+ (bordeaux-threads:make-lock))
(defparameter +log-stream-table-lock+ (bordeaux-threads:make-lock))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Log levels by package and function
;;;
;;; Each logging statement is associated with the package and function where it
;;; appears. Log levels can be defined per package and per function.If a
;;; function has no associated log-level, the log level of the package is used.

(defun log-level (category)
  (log-level% (reverse (cl-utilities:split-sequence #\: category))))

(defun log-level% (category)
  (loop
     :for cat :on category
     :for level = (gethash cat *log-level-ht*)
     :when level :do (return level) 
     :finally (return *default-log-level*)))

(defun set-log-level (category level)
  (bordeaux-threads:with-lock-held (+log-level-table-lock+)
    (setf (gethash (reverse (cl-utilities:split-sequence #\: category)) *log-level-ht*) level)))

(defsetf log-level set-log-level)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Log streams by 'category' (package and function)

(defun log-destination (category)
  (log-destination% (reverse (cl-utilities:split-sequence #\: category))))
  
(defun log-destination% (category)
  (loop
     :for cat :on category
     :for destination = (gethash cat *log-destination-ht*)
     :when destination
     :do (return destination)
     :finally (return *default-log-destination*)))

(defun set-log-destination (category destination)
  (bordeaux-threads:with-lock-held (+log-destination-table-lock+)
    (unless (or (stringp destination)
                (and (streamp destination)
                     (not (typep destination 'file-stream)))
                (eq destination t))
      (cl:error "Invalid destination"))
    (setf (gethash (reverse (cl-utilities:split-sequence #\: category)) *log-destination-ht*) destination)))
(defsetf log-destination set-log-destination)

(defun log-stream% (category)
  (let ((destination (log-destination% category)))
    (typecase destination
      (string (or (gethash destination *log-stream-ht*)
                  (setf (gethash destination *log-stream-ht*)
                        (open destination :direction :output
                              :if-does-not-exist *log-create-policy*
                              :if-exists *log-overwrite-policy*))))
      (symbol destination)
      (stream destination))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging messages


(defparameter *log-timestamp-format* '((:year 4) (:month 2) (:day 2) #\-
                                       (:hour 2) (:min 2) (:sec 2)))

(defparameter *stream-locks* (make-hash-table :test #'eq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Log file replacement
;;;
;;; - Close log file and rename by appending timestamp to its name
;;; - Open new file under original name

(defmacro message (level category formatter &rest args)
  (let ((rev-cat (reverse category)))
    `(when (log-p ',category ,level)
       (let ((timestamp
              (format-timestring nil (now) :format *timestamp-format* :timezone +utc-zone+)))
         (multiple-value-bind (result error)
             (ignore-errors
               (tagbody
                 :retry
                 (let ((stream (log-stream% ',category)))
                   (bordeaux-threads:with-lock-held ((get-stream-lock stream))
                     (when (typep stream 'file-stream)
                       (unless (open-stream-p stream)
                         (unless (probe-file stream)
                           (setf (gethash (log-destination% ',category) *log-stream-ht*)
                                 (open (pathname stream) :direction :output
                                       :if-does-not-exist *log-create-policy*
                                       :if-exists *log-overwrite-policy*)))
                         (go :retry))
                       (when (full-p stream)
                         (replace-file (log-destination% ',category) stream)
                         (go :retry)))
                     (format stream ,formatter timestamp
                             (aref +level-names+ ,level)
                             (current-thread-name)
                             ',rev-cat
                             ,@args)
                     (force-output stream)))))
           (if error
               (warn "Error ~a occurred during logging" error)
               result))))))

(defun log-p (category level)
  (and *logging*
       (<= level
           (log-level% category))))
    
(defun current-thread-name ()
  (bordeaux-threads:thread-name (bordeaux-threads:current-thread)))

(defun full-p (stream)
  (and (typep  stream 'file-stream)
       (>= (file-length stream) *max-log-file-bytes*)))

(defun replace-file (destination stream)
  (let* ((pathname
          (pathname stream))
         (old-name
          (pathname-name stream))
         (new-pathname
          (make-pathname :name (concatenate 'string
                                            old-name
                                            "-"
                                            (format-timestring nil (now) :format *log-timestamp-format*))
                         :defaults pathname)))
    (close stream)
    (rename-file pathname new-pathname)
    (setf (gethash destination *log-stream-ht*)
          (open pathname :direction :output
                :if-does-not-exist *log-create-policy*
                :if-exists *log-overwrite-policy*))))

(defun get-stream-lock (stream)
  (or (gethash stream *stream-locks* nil)
      (setf (gethash stream *stream-locks*)
              (bordeaux-threads:make-lock))))

(eval-when (:load-toplevel :compile-toplevel :execute)
(defmacro define-logging-macro (name severity)
  `(defmacro ,name (format &rest arguments &environment env)
     (let* ((fmt (concatenate 'string +prefix-format+ format "~&"))
            (blockname (enclosing-scope-block-name nil env))
            (category (cons (package-name *package*) blockname)))
       `(message ,,severity ,(reverse category) ,fmt ,@arguments))))
)

(define-logging-macro fatal +fatal+)
(define-logging-macro error +error+)
(define-logging-macro warning +warning+)
(define-logging-macro info +info+)
(define-logging-macro debug +debug+)
(define-logging-macro trace +trace+)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

