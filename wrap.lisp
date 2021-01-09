(cl:in-package :elcl/1)
(named-readtables:in-readtable :elcl)

(cl:defparameter *wrap-many-symbols-p* nil
  "If t, wrap as many wrappable Elisp symbols as possible. \"Wrappable\" means
  satisfying the Elisp predicate `elcl-wrappable-symbol-p'. This variable is
  used in the loop below to decide which Elisp symbols to wrap. It is
  recommended to set this to nil if the user just want to use some
  functionalities from Emacs (eg. some read commands to get user input),
  otherwise set to t for a bit of scripting Emacs with this library.")


(cl:defparameter *emacs-version* (eval '(symbol-value 'emacs-version)))


(cl:defparameter *wrap-these-symbols-also*
  '(symbol-value
    buffer-file-name
    completing-read
    custom-set-variables
    custom-set-faces
    read-file-name
    read-passwd
    require
    set
    user-error
    shell-command
    shell-command-to-string))


;; * utils
(cl:defun get-function-info-el (funname)
  "FUNNAME: the name of the function. This preserves the letter's case better
than the default case of the reader."
  ;; reference from Elisp's `help-fns--signature' code:
  (eval-string
   (format nil
           #str[
           (progn
             (require 'help-fns)
             (pcase-let* ((`(,real-function ,def ,_aliased ,real-def)
                            (help-fns--analyze-function '~A))
                          (doc (documentation '~A t)))
                         (list :arglist (help-function-arglist real-function)
                               :type (if (macrop '~A)
                                         "m" ; macro
                                         "f" ; function
                                         )
                               :doc doc
                               :real-def (format "%S" real-def)
                               :def (format "%S" def))))]
                               funname funname funname)))

(and t
     (loop
       for sym in *wrap-these-symbols-also*
       for symname = (string-downcase (symbol-name sym))
       for sym-in-elcl = (intern symname elcl/1::*elcl-package*)
       for sym-info = (get-function-info-el symname)
       for sym-type = (getf sym-info :type)
       for def-type = (if (and sym-type (eql (elt sym-type 0) #\m)) 'cl:defmacro 'cl:defun)
       for doc = (getf sym-info :doc)
       ;; must change the package to the current package so that alexandria can
       ;; properly parse the arglist symbols. without this, the package for the
       ;; arglist symbols would be `swank-io-package' and alexandria's function
       ;; `alexandria:parse-ordinary-lambda-list' won't parse this correctly:
       for arglist = (cl:mapcar 'cl:intern (cl:mapcar 'symbol-name (getf sym-info :arglist)))
       for applied-arglist = 
                           (cl:multiple-value-bind
                                 (requireds opt-triple rest-arg keyword-triple
                                  allow-other-keys-p aux-double key-p)
                               (alexandria:parse-ordinary-lambda-list arglist)
                             (cons
                              (cl:nconc requireds
                                        (cl:mapcar 'cl:car opt-triple)
                                        (cl:loop for triple in keyword-triple
                                           nconc (car (butlast triple))))
                              rest-arg))
                           ;; test: with '(A B c &optional x &key (ham 1 used-ham))
       do
          (cl:eval (cl:read-from-string
                    (cl:format nil "(~A:~A ~A::~A ~S 
                        (~A::~A
                          (format nil \"(~A ~~{~~(~~S~~)~~^ ~~})\" (append ~S ~S))))"
                               'cl def-type
                               'elcl sym arglist ; doc
                               'elcl/1 'eval-string
                               symname (cons 'list (car applied-arglist))
                               (cdr applied-arglist))))))


(cl:defun wrap (elsym &optional def-type newsym noclobber nowait)
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
           (wrap sym def-type nil noclobber nowait)
           (progn
             (check-type sym cons)
             (wrap (car sym)
                   (or (second sym) def-type)
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
(and nil
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
             )]))

;; <= `after-load-functions': makes Emacs to call Lisp to wrap all new symbols
;;   whenever Emacs has finished loading a library.


;; nullify these functions/macros in Elisp to make Lisp init file eval-able in
;; both Elisp and Lisp:
(and nil
     (eval-string
      #str[(dolist (sym '(cl:in-package named-readtables:in-readtable))
             (defalias sym 'ignore))]))

;; wrap all functions and macros symbols from Elisp without clobbering existing
;; Lisp symbols:
;;
;; *WARNING*: this will take a few minutes to complete:
(cl:when nil
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
       (insert (format ";; This file is automatically generated by %S\n"
                       (or load-file-name buffer-file-name (buffer-name))))
       (insert (format "(cl:setq %s::%s %S)\n" 'elcl '*emacs-version* emacs-version))
       (insert (format "(and %s\n" (slime-eval '(cl:symbol-value elcl/1::*wrap-many-symbols-p*))))
       (elcl-print-list-full my-macros t)
       (elcl-print-list-full my-functions)
       (elcl-print-list-full my-special-forms)
       (elcl-print-list-full my-subrs)
       (insert ")\n")
       ;; (elcl-print-list-full my-variables)
       ;; (elcl-print-list-full my-other)
       ))]))

(cl:when nil
  (eval `(setq elcl-wrap-many-symbols-p ',elcl/1::*wrap-many-symbols-p*))
  ;; (eval-string
  ;;  #str[(let* ((regexp
  ;;                (slime-eval `(cl:format nil "^\\(~{~(~A~)~^\\|~}\\)"
  ;;                                        '(custom- shell- find- load- cl-))))
  ;;              result)
  ;;         (do-all-symbols (item (elcl-lisp-wrap-symbols-maybe result t))
  ;;   		(and
  ;;            (elcl-wrappable-symbol-p item)
  ;;            ;; - alternatively, one can wrap only a few symbols are needed for
  ;;            ;;   my init, so only a handful of symbols are selected here:
  ;;            (or elcl-wrap-many-symbols-p (string-match-p regexp (symbol-name item)))
  ;;            (push item result))))])
  )
