(in-package :cl-user)

(asdf:defsystem :elshim
  :version      "0.0.1"
  :description  "description"
  :author       "Hai NGUYEN <haiuyeng@gmail.com>"
  :serial       t
  :license      "GPL, version 3"
  :components   ((:file "elshim-emacs")
                 (:file "elshim"))
  :depends-on   (#:swank #:cl-ppcre))
