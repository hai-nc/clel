(in-package :cl)

(asdf:defsystem :elcl
  :version      "0.2.1"
  :description  "description"
  :author       "Hai NGUYEN <haiuyeng@gmail.com>"
  :serial       t
  :license      "GPLv3"
  :components   ((:file "package")
                 (:file "core")
                 (:file "wrap")
                 (:file "compat"))
  :depends-on   (#:swank #:alexandria #:cl-ppcre #:named-readtables))
