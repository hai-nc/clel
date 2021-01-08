(in-package :cl)

(asdf:defsystem :clel
  :version      "0.0.1"
  :description  "description"
  :author       "Hai NGUYEN <haiuyeng@gmail.com>"
  :serial       t
  :license      "GPLv3"
  :components   ((:file "package")
                 (:file "core")
                 (:file "wrapper")
                 (:file "import")
                 (:file "compat"))
  :depends-on   (#:swank #:cl-ppcre #:named-readtables))
