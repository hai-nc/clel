(cl:in-package :elcl/1)
(named-readtables:in-readtable :elcl)

;; ** keys

;; *** smart-tab

;; from Malabarba (2015)
;; http://endlessparentheses.com/define-context-aware-keys-in-emacs.html:
;; ensure the keymap used here is not overriden by other keymaps:
(eval-string
 #str[(progn
        (defmacro endless/define-conditional-key (keymap key def
                                                  &rest body)
          "In KEYMAP, define key sequence KEY as DEF conditionally.
  This is like `define-key', except the definition
  \"disappears\" whenever BODY evaluates to nil."
          (declare (indent 3)
                   (debug (form form form &rest sexp)))
          `(define-key ,keymap ,key
             '(menu-item
               ,(format "maybe-%s" (or (car (cdr-safe def)) def))
               nil
               :filter (lambda (&optional _)
                         (when ,(macroexp-progn body)
                           ,def)))))
        (endless/define-conditional-key prog-mode-map (kbd "<tab>")
                                        'hs-hide-level
                                        ;; Lisp choke on ?\(, so put this whole
                                        ;; thing inside a string:
                                        (eq ?\( (char-after))))])

;; ** clipmon-mode
(when (fboundp 'clipmon-mode)
  ;; (when x-select-enable-clipboard-manager (clipmon-mode-start)) ; fix for yasnippet as well
  (clipmon-mode -1)
  (message "Turning off clipmon-mode to avoid error \"(void-variable clipmon--autoinsert)\" <= redisplay_internal\\ \\(C\\ function\\)()"))

;; ** mode-line

;; ** outline
;; *** hideshow,hs
(add-hook 'prog-mode-hook 'hs-minor-mode t)
(add-hook 'prog-mode-hook 'hs-hide-initial-comment-block t)

;; ** notify

;; * utils
;; ** frame
;; (defun my-frame-switch (frame-name)
;;   (interactive "\i")
;;   (ignore-errors (select-frame-by-name NAME)))

;; ** navigation

;; FIXME!
;; (defun-cl my-goto-doc (filename)
;;   (interactive (list (elcl:read-file-name "Document: ")))
;;   (defvar *frame-doc-name* "doc")
;;   (select-frame-by-name *frame-doc-name*)
;;   (find-file filename))


;; * package
;; ** quicklisp
(defun my-quicklisp-quickload ()
  (interactive)
  (slime-quicklisp-quickload (slime-read-quicklisp-system-name)))

;; * sysadmin

;; ** root command

;; * clipboard


;; * bibtex
;; based on guide in https://github.com/tmalsburg/helm-bibtex

;; ** setup

(autoload 'helm-bibtex "helm-bibtex" "" t)

(eval ; use `elcl:eval' to shove the Lisp variables inside:
 `(setq bibtex-completion-bibliography "~/db.bib"
        ;; folder storing the pdf files of the bib articles:
        bibtex-completion-library-path "~/pdf/"
        ;; ;; use pdf viewer other than Emacs:
        ;; bibtex-completion-pdf-open-function
        ;; (lambda (fpath)
        ;;   (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath))
        ))


;; ** search
;; (require 'helm-bibtex)
