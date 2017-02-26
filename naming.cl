;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-02-23 21:17:08>

;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2012, Max Mikhanosha. All rights reserved.
;;;
;;; This file is licensed to You under the Apache License, Version 2.0
;;; (the "License"); you may not use this file except in compliance
;;; with the License.  You may obtain a copy of the License at
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "LOG2")

(defun fix-method-spec-list (spec)
  "Flatten method specializer list, remove any T specializers, replace
all constant EQL specializers with their values, and eliminate
non-constant ones"
  (let (list)
    (labels ((traverse (spec)
               (cond
                 ((null spec))
                 ((and (consp spec)
                       (eq 'eql (first spec))
                       (endp (cddr spec)))
                  (when (constantp (second spec)) 
                    (let ((result (eval (second spec))))
                      (when (atom result)
                        (push result list)))))
                 ((consp spec)
                  (traverse (first spec))
                  (traverse (rest spec)))
                 ((eq spec T))
                 ((typep spec 'built-in-class)
                  (traverse (class-name spec)))
                 #+ccl ((typep spec 'ccl:eql-specializer)
                        (traverse (ccl:eql-specializer-object spec)))
                 ((typep spec 'class)
                  (traverse (class-name spec)))
                 ((atom spec)
                  (push spec list)))))
      (traverse spec)
      (nreverse list))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
