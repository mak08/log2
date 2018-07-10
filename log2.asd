;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2018-07-10 20:44:53>

(defsystem "log2"
  :description "log2: simple logging."
  :version "0.0.1"
  :author "Michael Kappert"
  :licence "GNU GPLv3 / Apache"
  :default-component-class cl-source-file.cl
  :depends-on ()
  :components ((:file "log2-package")
               (:module "naming"
                        :pathname ""
                        :depends-on ("log2-package")
                        :components ((:file "naming")
                                     #+sbcl (:file "naming-sbcl")
                                     #+ccl (:file "naming-ccl")))
               (:file "log2-impl" :depends-on ("naming"))
               (:module "timers"
                        :pathname ""
                        :depends-on ("log2-impl")
                        :components ((:file "timers")))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
