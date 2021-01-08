(cl:in-package :elcl/1)
(named-readtables:in-readtable :elcl)

(cl:defparameter *wrap-many-symbols-p* nil
  "If t, wrap as many wrappable Elisp symbols as possible. \"Wrappable\" means
  satisfying the Elisp predicate `elcl-wrappable-symbol-p'. This variable is
  used in the loop below to decide which Elisp symbols to wrap. It is
  recommended to set this to nil if the user just want to use some
  functionalities from Emacs (eg. some read commands to get user input),
  otherwise set to t for a bit of scripting Emacs with this library.")

(cl:defparameter *wrap-these-symbols-also*
  '(and or if cond unless when while
    progn prog1 add-to-list autoload add-hook completing-read require set
    user-error shell-command shell-command-to-string))


;; * wrappers of elisp
(cl:defun wrap (elsym &optional macrop newsym noclobber nowait)
  "Wraps the Elisp function/macro symbol ELSYM in a Lisp symbol of the same name.

If the wrapped Elisp symbol is a function, this generates a function, and a
macro for a macro.

The generated Lisp function/macro takes an arbitrary number of arguments and
these arguments will be evaluated in Lisp and then passed along to Emacs.

NEWSYM: assign a new symbol name instead of the wrapped Elisp symbol."
  (cond
    ((consp elsym)
     (dolist (sym elsym)
       (if (atom sym)
           (wrap sym macrop nil noclobber nowait)
           (progn
             (check-type sym cons)
             (wrap (car sym)
                   (or (second sym) macrop)
                   (third sym)
                   (or (fourth sym) noclobber)
                   (or (fifth sym) nowait))))))
    ((and (symbolp elsym)
          ;; no clobber
          (let* ((elsym-in-elcl (cl:find-symbol (symbol-name elsym) *elcl-package*)))
            (or (null elsym-in-elcl) ; either elsym-in-elcl has not yet been created
                (null (fboundp elsym-in-elcl) ; or it has not yet been fbound
                                                ; or macro-bound
                          )
                (null noclobber) ;  or noclobber is nil
                )))
     (let* ((def-fun (if t
			             'cl:defmacro
                         'cl:defun))
            (newsym (intern (symbol-name (or newsym elsym)) *elcl-package*)))
       ;; (cl:shadow newsym pkg)
       (cl:export newsym *elcl-package*)
       (cl:eval (if (cl:eq def-fun 'cl:defmacro)
                    `(,def-fun ,newsym (&rest _args)
                       `(eval-string (format nil "(~(~A~) ~{~S~^ ~})" ,'',elsym ',_args)
                                     nil ; str-result
                                     ,',nowait))
                    `(,def-fun ,newsym (&rest _args)
                       (eval-string (format nil "(~(~A~) ~{'~S~^ ~})" ',elsym _args)
                                    nil ; str-result
                                    ,nowait))))))))

;; ** simple

;; helper
(eval-string
 #str[(progn
        (defun elcl-wrappable-symbol-p (item)
          (and (symbolp item)
               (or (macrop item)
                   (functionp item)
                   (subrp item))
			   (null ; exclude symbols with -- (internal functions/macros) and
                                        ;   those with ":" (which chokes Lisp as it already use 'elcl
                                        ;   as the package of the wrapping symbol):
				(string-match-p  "[^-[:alnum:]]\\|--\\|:"
						         (symbol-name item)))))
        
        (defun elcl-lisp-eval-string (str)
          (slime-eval `(cl:eval (cl:read-from-string ,str))))
        
        (defun elcl-lisp-wrap-symbols-maybe (syms &optional noclobber)
          (require 'cl-lib)
          (let* (sym-subset)
            ;; Breaking list of syms into smaller chunk to send to Swank for
            ;;   easier debugging if some symbols cause troubles: The list of
            ;;   syms can get crazy long and without breaking the list into
            ;;   smaller chunk like this the Swank server get the wrong string
            ;;   to work with, perhaps due to the oversize string:
            (while syms
                   (setq sym-subset nil)
                   (dotimes (i 5000)
                     (and syms (push (pop syms) sym-subset)))
                   (slime-eval `(cl:let ((cl:*package* (cl:find-package 'elcl)))
		                          (elcl/1::wrap ',sym-subset nil ,noclobber))))))

        (add-hook 'after-load-functions
                  (defun elcl--after-load-function (fullname)
                    "Wrap all symbols from the package whose absolute name is FULLFNAME
FULLFNAME: the absolute filename of the library just loaded."
                    (let* (result)
                      (dolist (item
                               (assoc fullname load-history)
                               (elcl-lisp-wrap-symbols-maybe result t))
                        (and (elcl-wrappable-symbol-p item)
                             (push item result)))))
                  t)
        )])

;; <= `after-load-functions': makes Emacs to call Lisp to wrap all new symbols
;;   whenever Emacs has finished loading a library.


;; nullify these functions/macros in Elisp to make Lisp init file eval-able in
;; both Elisp and Lisp:
(eval-string
 #str[(dolist (sym '(cl:in-package named-readtables:in-readtable))
        (defalias sym 'ignore))])

;; wrap all functions and macros symbols from Elisp without clobbering existing
;; Lisp symbols:
;;
;; *WARNING*: this will take a few minutes to complete:
(cl:when t
  (eval-string
   #str[
   (let ((my-macros nil)
         (my-functions nil)
         (my-special-forms nil)
         (my-subrs nil)
         (my-variables nil)
         (my-other nil)
         name)
     (defun elcl-print-list-full (lis &optional macrop)
       "Default print will limit long list with ..., so use this function instead
to print a list in full."
       (require 'cl-lib)
       (and lis
            (cl-loop initially
                     do (insert (format "(%s:wrap '(\n" 'elcl/1))
                     for item in lis
                     ;; forget about symbols which may choke the
                     ;; Lisp reader:
                     unless (string-match-p "[()|: ]" (symbol-name item))
                     do (insert (format "%s\n" item))
                     finally
                     do (insert (format ") %s)\n" macrop)))))
     (do-all-symbols (sym)
       ;; (setq name (symbol-name sym))
       (cond
         ((subrp sym) (push sym my-subrs))
         ((special-form-p sym) (push sym my-special-forms))
         ((macrop sym) (push sym my-macros))
         ((functionp sym) (push sym my-functions))
         ((boundp sym) (push sym my-variables))
         (t (push sym my-other))))
     ;; ;; test
     ;; (elcl-print-list-full "special-forms" my-special-forms)

     (with-temp-file "import.lisp"
       ;; (insert (format "(cl:in-package :%s)\n" 'elcl))
       ;; (insert (format "(named-readtables:in-readtable :%s)\n" 'elcl))
       (insert (format "(cl:setq %s::%s %S)\n" 'elcl '*emacs-version* emacs-version))
       (elcl-print-list-full my-macros t)
       (elcl-print-list-full my-functions)
       (elcl-print-list-full my-special-forms)
       (elcl-print-list-full my-subrs)
       ;; (elcl-print-list-full my-variables)
       ;; (elcl-print-list-full my-other)
       ))]))

(cl:when nil
  (eval `(progn
           (setq elcl-wrap-many-symbols-p ',elcl/1::*wrap-many-symbols-p*
                 elcl-wrap-these-symbols-also ',elcl/1::*wrap-these-symbols-also*)
          ;; wrap these important symbols first:
          (elcl-lisp-wrap-symbols-maybe elcl-wrap-these-symbols-also t)))
  (eval-string
   #str[(let* ((regexp
                 (slime-eval `(cl:format nil "\\(^\\(~{~(~A~)~^\\|~}\\)\\|~(~A~)\\)"
                                         '(def do read- with- save- symbol- custom-
                                           shell- find- load- cl-)
                                         '-mode)))
               result)
          (do-all-symbols (item (elcl-lisp-wrap-symbols-maybe result t))
			(and
             (elcl-wrappable-symbol-p item)
             ;; - alternatively, one can wrap only a few symbols are needed for
             ;;   my init, so only a handful of symbols are selected here:
             (or elcl-wrap-many-symbols-p (string-match-p regexp (symbol-name item)))
             (push item result))))]))
