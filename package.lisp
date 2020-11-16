(in-package :cl)

(pushnew :elshim *features*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (named-readtables:find-readtable :elshim)
    (named-readtables:defreadtable :elshim
      (:merge :standard)
      ;; BELOW is NOT NEEDED!?, since the Swank server already downcase every symbol it
      ;; sends to Emacs:
      (:case #.(if (eql :upcase (readtable-case *readtable*))
                   :invert
                   (readtable-case *readtable*)))
      )))

(named-readtables:in-readtable :elshim)


(defpackage elshim/1
  (:documentation "Utilities to wrap Elisp symbols in Lisp.")
  (:use #:cl)
  (:shadow eval defun)
  (:export
   #:defun
   #:*current-prefix-arg*
   #:eval
   #:eval-string
   #:map-quote
   #:wrap
   #:form->string))

(defpackage elshim
  (:documentation "The interface containing Lisp symbols that wraps Elisp
  symbols of the same names.")
  (:import-from #:cl #:nil #:t) ; avoid package cl: prefix show up
                                ; eg. common-lisp:nil while Lisp reader (being
                                ; in the :elshim namespace) reads the form to
                                ; send to Emacs
  (:shadowing-import-from #:elshim/1 #:eval)
  (:export
   #:clipboard-yank
   #:eval
   #:with-current-buffer))

