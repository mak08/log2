;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; System         
;;; Author         
;;; Copyright      (c)  2004
;;; Created        13/11/2004 22:03:39
;;; Last Modified  <michael 2017-03-07 23:41:23>
;;; Description
;;; $Id$

(defpackage "LOG2"
  (:use "COMMON-LISP" "LOCAL-TIME")
  (:shadow "ERROR" "DEBUG" "WARNING" "TRACE")
  (:export "*LOG-STREAM*"
           "*LOGGING*"
           "LOG-LEVEL"
           "ERROR"
           "WARNING"
           "INFO"
           "DEBUG"
           "TRACE"
           "+ERROR+"
           "+WARNING+"
           "+INFO+"
           "+DEBUG+"
           "+TRACE+"
           "MESSAGE"))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
