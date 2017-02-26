;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; System         
;;; Author         
;;; Copyright      (c)  2004
;;; Created        13/11/2004 22:03:39
;;; Last Modified  <michael 2017-02-22 21:00:17>
;;; Description
;;; $Id$

(defpackage "LOG2"
  (:use "COMMON-LISP" "LOCAL-TIME")
  (:shadow "ERROR" "DEBUG" "WARNING" "TRACE")
  (:export "*LOG-STREAM*"
           "*LOGGING*"
           "*LOG-LEVEL*"
           "*LOG-PREFIX*"
           "*CATEGORY*"
           "CATEGORY"
           "ERROR"
           "WARNING"
           "INFO"
           "DEBUG"
           "TRACE"
           "MESSAGE"))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
