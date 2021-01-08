(cl:in-package :clel)
(named-readtables:in-readtable :clel)


;; * UI
;; ** GUI
(custom-set-variables
 '(blink-cursor-mode nil)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 ;; window
 '(same-window-regexps '("\\*slime-"))      ; ((("^[^ ]"))(nil))
 '(same-window-buffer-names '("*Help*" "*Messages*")) ; ((nil))
 '(help-window-select t)
 '(switch-to-buffer-preserve-window-point 'already-displayed)
 ;; GUI
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(tooltip-resize-echo-area t)
 '(frame-title-format (quote ("<%b>%n%e%t %I (%F)")))
 ;; important: turn off this setting to prevent Emacs's freezing when quitting
 ;; GUI Emacs in Debian XFCE4:
 '(x-select-enable-clipboard-manager nil)
 '(select-enable-clipboard t)
 '(select-enable-primary nil)
 '(x-select-enable-primary t)   ; for emacs before 25.1
 '(x-select-enable-clipboard t)
 '(select-enable-clipboard t)
 ;; probably unecessary!?:
 '(save-interprogram-paste-before-kill t)
 '(yank-pop-change-selection t))

;; ** font
(cond
  ((eq window-system 'x)
   (custom-set-faces
    '(default ((t
                :foundry "PfEd"
                :family "DejaVu Sans Mono"
                :height 160
                :foreground "white smoke" ; (("gray90")("#2E4E2E")("#28593A")("#2D6542"))
                :background "#727246" ; (("LightGoldenrod4")("#75744D")("#466E39")("#75744B"))
                )))
    '(font-lock-comment-face ((t (:foreground "tan"))))))
  ((eq system-type 'windows)
   (set-face-attribute 'default
                       nil
                       :family "Bitstream Vera Sans Mono" ; "andale mono"
                       ;; :foundry "outline"
                       :width 'medium ; ((condensed))
                       :weight 'medium
                       :height 170
                       :foreground "#DEDEDE"
                       :background "#063016")))

;; ** frame

(fringe-mode '(6 . 4))

(dolist (param '(fullscreen)) ; width height background-color foreground-color
        (add-to-list 'frame-inherited-parameters param t))

(progn ; ensure eval synchronously so that `my-frame-maximize' exists
              ; before being funcalled
        (defun my-frame-maximize ()
          "."
          (interactive)
          (set-frame-parameter nil 'fullscreen 'maximized))
        (my-frame-maximize))


;; ** keys

;; *** smart-tab

;; from Malabarba (2015)
;; http://endlessparentheses.com/define-context-aware-keys-in-emacs.html:
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

;; ensure the keymap used here is not overriden by other keymaps:
(eval-string
 #str[(endless/define-conditional-key prog-mode-map (kbd "<tab>")
                                      'hs-hide-level
                                      ;; Lisp choke on ?\(, so put this whole
                                      ;; thing inside a string:
                                      (eq ?\( (char-after)))])

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
;;   (interactive (list (clel:read-file-name "Document: ")))
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

(eval ; use `clel:eval' to shove the Lisp variables inside:
 `(setq bibtex-completion-bibliography "~/db.bib"
        ;; folder storing the pdf files of the bib articles:
        bibtex-completion-library-path "~/pdf/"
        ;; ;; use pdf viewer other than Emacs:
        ;; bibtex-completion-pdf-open-function
        ;; (lambda (fpath)
        ;;   (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath))
        ))


;; ** search
(require 'helm-bibtex)
