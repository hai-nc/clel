(in-package :cl)

(pushnew :elcl *features*)

;; inspired by code from the Parenscript library (2020):
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (named-readtables:find-readtable :elcl)
    (named-readtables:defreadtable :elcl
      (:merge :standard)
      (:case #.(if (eql :upcase (readtable-case *readtable*))
                   :invert
                   (readtable-case *readtable*))))))

(named-readtables:in-readtable :elcl)


(defpackage elcl/1
  (:documentation "Utilities to wrap Elisp symbols in Lisp.")
  (:use #:cl)
  (:shadow eval defun)
  (:export
   #:defun
   #:current-prefix-arg
   #:eval
   #:eval-string
   #:map-quote
   #:wrap
   #:form->string))

(defpackage elcl
  (:documentation "The namespace containing all Lisp symbols that wraps Elisp
  symbols of the same names. All Lisp symbols in this package are
  external (ie. exported from this package).")
  (:import-from #:cl #:nil #:t) ; avoid package cl: prefix show up
                                ; eg. common-lisp:nil while Lisp reader (being
                                ; in the :elcl namespace) reads the form to
                                ; send to Emacs
  (:shadowing-import-from #:elcl/1 #:eval #:eval-string)
  (:export
   #:clipboard-yank
   #:eval
   #:with-current-buffer))

