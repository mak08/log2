;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; System         
;;; Author         
;;; Copyright      (c)  2004
;;; Created        13/11/2004 22:03:39
;;; Last Modified  <michael 2020-11-06 23:59:11>
;;; Description
;;; $Id$

(defpackage "LOG2"
  (:use "COMMON-LISP" "LOCAL-TIME")
  (:shadow "ERROR" "DEBUG" "WARNING" "TRACE")
  (:export "*DEFAULT-LOG-STREAM*"
           "*LOGGING*"
           "LOG-LEVEL"
           "LOG-DESTINATION"
           "ERROR"
           "WARNING"
           "INFO"
           "DEBUG"
           "TRACE"
           "TRACE-MORE"
           "+ERROR+"
           "+WARNING+"
           "+INFO+"
           "+DEBUG+"
           "+TRACE+"
           "+TRACE-MORE+"
           "MESSAGE"))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
