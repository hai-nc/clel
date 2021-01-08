(cl:in-package :clel)
(named-readtables:in-readtable :clel)

;; This file contain Lisp symbols that needs modify to achieve the same or
;; similar effect to their Elisp counterparts:

(eval-string
 #str[
 (progn
   (require 'cl-lib)
   (defun clel-def--before (&rest args)
     (clel-lisp-eval-string (format "(clel/1:wrap '%s)" (car args))))
   (cl-loop for sym in '(defmacro defun defalias)
            do (advice-add sym :before 'clel-def--before)))])

;; * hooks
(cl:defun clel::remove-hook (hook fun &rest _)
  "Matching with `add-hook' syntax for easy swapping."
  (eval `(remove-hook ',hook ',fun)))

;; * buffers
;; Elisp pointers to UI objects such as the frames, windows, and buffers cannot
;; be read by the CL image so these need indirect accessing, like so:
;;
;; (with-current-buffer nil default-directory)
(cl:defun clel::current-buffer-name ()
  "Elisp `current-buffer' cannot be used because it will point to the slime
connection one."
  (eval '(buffer-name (window-buffer (selected-window)))))

(cl:defmacro clel::with-current-buffer (buffer cl:&rest body)
  "BUFFER: name of a target Emacs buffer to temporarily switch to."
  `(clel/1:eval
    `(with-current-buffer (or ,',buffer (current-buffer-name))
      ,',@body)))

(cl:defun clel::clipboard-yank ()
  "Temporarily reset the current buffer from that of slime connection to the one
in the currently selected window for `clipboard-yank'."
  (clel::with-current-buffer nil (clipboard-yank)))
