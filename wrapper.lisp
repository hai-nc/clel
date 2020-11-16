(cl:in-package :elshim/1)
(named-readtables:in-readtable :elshim)

;; * wrapper
;; ** simple

;; helper
;; define `elshim-slime-eval-string' on Emacs:
(elshim/1:eval-string
 #str[(progn
        (defun elshim-wrappable-symbol-p (item)
          (and (symbolp item)
               (or (macrop item)
                   (functionp item)
                   (subrp item))
			   (null ; exclude symbols with -- (internal functions/macros) and
                     ;   those with ":" (which chokes Lisp as it already use 'elshim
                     ;   as the package of the wrapping symbol):
				(string-match-p  "[^-[:alnum:]]\\|--\\|:"
						        (symbol-name item)))))
        
        (defun elshim-lisp-eval-string (str)
          (slime-eval `(cl:eval (cl:read-from-string ,str))))
        
        (defun elshim-lisp-wrap-symbols-maybe (syms &optional noclobber)
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
                   (slime-eval `(cl:let ((cl:*package* (cl:find-package 'elshim)))
		                          (elshim/1::wrap ',sym-subset ,noclobber))))))

        (add-hook 'after-load-functions
                  (defun elshim--after-load-function (fullname)
                    "FULLFNAME: the absolute filename of the library just
                  loaded."
                    (let* (result)
                      (dolist (item
                               (assoc fullname load-history)
                               (elshim-lisp-wrap-symbols-maybe result))
                        (and (elshim-wrappable-symbol-p item)
                             (push item result)))))
                  t))])

;; <= `after-load-functions': makes Emacs to call Lisp to wrap all new symbols
;;   whenever Emacs has finished loading a library.


;; ;; NO NEED
;; ;; import all the symbols into elshim first, so that all Elisp symbols are
;; ;; recognized in elshim script:
;; (elshim/1:eval-string
;;    #str[(let* (result)
;;           (do-all-symbols (item
;;                            (slime-eval `(cl:mapcar
;;                                            (cl:lambda (sym)
;;                                              (cl:intern (cl:symbol-name sym) elshim/1::+elshim-package+))
;;                                            ',result)))
;; 			(and (elshim-wrappable-symbol-p item)
;;                  (push item result))))])


;; wrap functions and macros symbols from Elisp without clobbering existing
;; Lisp symbols:
;;
;; TIME-CONSUMING: this takes a few minutes to wrap all the symbols, and only a
;; few symbols are needed for my init, so skip this:
(and nil
  (elshim/1:eval-string
   #str[(let* (result)
          (do-all-symbols (item (elshim-lisp-wrap-symbols-maybe result))
			(and (elshim-wrappable-symbol-p item)
                 (push item result))))]))

;; These functions and macros wrap the Elisp code in their body. See
;; `elshim:wrap' for the behavior of these wrapping functions and macros:
(cl:loop for item in '(add-to-list autoload add-hook completing-read custom-set-variables
                       custom-set-faces defun find-file fringe-mode
                       read-file-name read-passwd require select-frame-by-name set
                       user-error shell-command shell-command-to-string)
   if (atom item)
     do (elshim/1:wrap item)
   else
     do (elshim/1:wrap (car item) (cdr item)))
;; (loop for item in (list (cons (format nil "^\(~{~(~A~)~^\|~}\)"
;;                                                     '(def read- with- save- symbol-))))
;;       do (loop for sym in (elshim/1:eval
;;                            `(let* (result)
;;                               (cl-do-symbols (item)
;;                                              (and (symbolp item)
;;                                                   (or (macrop item)
;;                                                       (functionp item)
;;                                                       (subrp item))
;;                                                   (string-match-p ,regexps (symbol-name item))
;;                                                   (push item result)))
;;                               result))
;;                do (elshim/1:wrap sym pkgsym)))


;; ** special cases

;; *** remove-hook
;; (elshim/1:defun remove-hook (hook fun &rest _)
;;   "Matching with `add-hook' syntax for easy swapping."
;;   (eval `(remove-hook ',hook ',fun)))


;; *** with-current-buffer
;; Elisp pointers to UI objects such as the frames, windows, and buffers cannot
;; be read by the CL image so these need indirect accessing, like so:
;;
;; (with-current-buffer nil default-directory)
(cl:defun elshim::current-buffer-name ()
  "Use this instead of calling `current-buffer', which will point to the swank ones."
  (eval '(buffer-name (window-buffer (selected-window)))))

(cl:defmacro elshim::with-current-buffer (buffer cl:&rest body)
  "BUFFER: name of a target Emacs buffer to temporarily switch to."
  `(elshim/1:eval
    `(with-current-buffer (or ,',buffer (current-buffer-name))
      ,',@body)))

(cl:defun elshim::clipboard-yank ()
  (elshim::with-current-buffer nil (clipboard-yank)))
