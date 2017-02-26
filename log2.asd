;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-02-23 23:27:44>

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
               (:file "log2-impl" :depends-on ("naming"))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
