(cl:in-package :elcl/1)
(named-readtables:in-readtable :elcl)

;; This file contain Lisp symbols that needs modify to achieve the same or
;; similar effect to their Elisp counterparts:

(and nil
     (eval-string
      #str[
      (progn
        (require 'cl-lib)
        (defun elcl-def--before (&rest args)
          (elcl-lisp-eval-string (format "(elcl/1:wrap '%s)" (car args))))
        ;; (cl-loop for sym in '(defmacro defun defalias)
        ;;          do (advice-add sym :before 'elcl-def--before))
        )]))

;; * hooks
(cl:defun elcl::remove-hook (hook fun &rest _)
  "Matching with `add-hook' syntax for easy swapping."
  (eval `(remove-hook ',hook ',fun)))

;; * buffers
;; Elisp pointers to UI objects such as the frames, windows, and buffers cannot
;; be read by the CL image so these need indirect accessing, like so:
;;
;; (with-current-buffer nil default-directory)
(cl:defun elcl::current-buffer-name ()
  "Elisp `current-buffer' cannot be used because it will point to the slime
connection one."
  (eval '(buffer-name (window-buffer (selected-window)))))

(cl:defmacro elcl::with-current-buffer (buffer cl:&rest body)
  "BUFFER: name of a target Emacs buffer to temporarily switch to."
  `(elcl/1:eval
    `(with-current-buffer (or ,',buffer (current-buffer-name))
      ,',@body)))

(cl:defun elcl::clipboard-yank ()
  "Temporarily reset the current buffer from that of slime connection to the one
in the currently selected window for `clipboard-yank'."
  (elcl::with-current-buffer nil (clipboard-yank)))
