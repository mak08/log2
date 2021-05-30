;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Simple logging module
;;; Created        29/06/2003 00:13:40
;;; Last Modified  <michael 2021-04-30 21:03:35>

(in-package "LOG2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

(defparameter +level-names+ (make-array 7
                                        :element-type 'string
                                        :initial-contents #("FATAL" "ERROR" "WARNING" "INFO" "TRACE" "TRACE-MORE" "DEBUG")))
(declaim (type (simple-array string 1) +level-names+))

(defparameter +fatal+ 0)
(defparameter +error+ 1)
(defparameter +warning+ 2)
(defparameter +info+ 3)
(defparameter +trace+ 4)
(defparameter +trace-more+ 5)
(defparameter +debug+ 6)

(defparameter +prefix-format+ "~a [~7@a] <~a> ~{~a~^:~}~,8T")

(defparameter *logging* t)

(defparameter *log-create-policy* :create)
(defparameter *log-overwrite-policy* :append)
(defparameter *max-log-file-bytes* 20000000)

(defparameter *timestamp-format* '((:year 4) #\- (:month 2) #\- (:day 2) #\space
                                   (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:msec 3)))

(defparameter *default-log-level* +info+)
(defparameter *log-level-ht* (make-hash-table :test #'equalp))
(defvar +log-level-table-lock+ (bordeaux-threads:make-lock "LEVEL-TABLE-LOCK"))

(defparameter *default-log-destination* T)
(defparameter *log-destination-ht* (make-hash-table :test #'equalp)
  "Category -> Destination")
(defparameter *log-stream-ht* (make-hash-table :test #'equalp)
  "Destination -> Stream")
(defparameter +log-destination-table-lock+ (bordeaux-threads:make-lock "DESTINATION-TABLE-LOCK"))
(defparameter +log-stream-table-lock+ (bordeaux-threads:make-lock "STREAM-TABLE-LOCK"))


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
    (etypecase destination
      (string (or (gethash destination *log-stream-ht*)
                  (setf (gethash destination *log-stream-ht*)
                        (open destination :direction :output
                              :if-does-not-exist *log-create-policy*
                              :if-exists *log-overwrite-policy*))))
      (symbol
       (assert (eq destination t))
       *standard-output*))))

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

(defun log-p (category level)
  (and *logging*
       (<= level
           (log-level% category))))
    
(defun current-thread-name ()
  (bordeaux-threads:thread-name (bordeaux-threads:current-thread)))

(defun full-p (stream)
  (and (typep  stream 'file-stream)
       (>= (file-length stream) *max-log-file-bytes*)))

(defun get-destination-lock (destination)
  (or (gethash destination *destination-locks* nil)
      (setf (gethash destination *destination-locks*)
            (typecase destination
              (symbol
               (bordeaux-threads:make-lock (format nil "~a" destination)))
              (t
               (bordeaux-threads:make-lock destination))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging messages


(defparameter *log-timestamp-format* '((:year 4) (:month 2) (:day 2) #\-
                                       (:hour 2) (:min 2) (:sec 2) #\+ (:usec 4)))

(defparameter *destination-locks* (make-hash-table :test #'eq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Log file replacement
;;;
;;; - Close log file and rename by appending timestamp to its name
;;; - Open new file under original name

(defmacro message (level category formatter &rest args)
  (let ((rev-cat (reverse category))
        (ts-var (gentemp "TS-"))
        (stream-var (gentemp "STREAM-"))
        (dest-var (gentemp "DEST-")))
    `(when (log-p ',category ,level)
       (let ((*print-pretty* nil)
             (,ts-var
              (format-timestring nil (now) :format *timestamp-format* :timezone +utc-zone+)))
         (multiple-value-bind (result error)
             (ignore-errors
               (let ((,dest-var (log-destination% ',category)))
                 (bordeaux-threads:with-recursive-lock-held ((get-destination-lock ,dest-var))
                   (let ((,stream-var (log-stream% ',category)))
                     (when (typep ,stream-var 'file-stream)
                       (unless (probe-file ,stream-var)
                         ;; Deleting the underlying file would cause log entries to be lost:
                         ;; Entries are written to a buffer until the OS flushes,
                         ;; whereupon they are silently discarded. 
                         (setf (gethash (log-destination% ',category) *log-stream-ht*)
                               (open (pathname ,stream-var) :direction :output
                                     :if-does-not-exist *log-create-policy*
                                     :if-exists *log-overwrite-policy*))
                         (setf ,stream-var (log-stream% ',category)))
                       (when (full-p ,stream-var)
                         (replace-file (log-destination% ',category) ,stream-var)
                         (setf ,stream-var (log-stream% ',category))))
                     (format ,stream-var ,formatter ,ts-var
                             (aref +level-names+ ,level)
                             (current-thread-name)
                             ',rev-cat
                             ,@args)
                     (force-output ,stream-var)))))
           (if error
               (cl:warn "Error ~a occurred during logging" error)
               result))))))

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
(define-logging-macro trace +trace+)
(define-logging-macro trace-more +trace-more+)
(define-logging-macro debug +debug+)


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

