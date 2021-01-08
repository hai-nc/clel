(cl:in-package :clel/1)
(named-readtables:in-readtable :clel)

(cl:defparameter *version* "0.1.1")

(cl:defparameter *clel-package* (find-package 'clel) "A convenient thunk.")

;; * transpile:lisp->elisp

;; ** reader macro
;;
;; A convenient reader macro to generate a string verbatim from the text of the
;;   source code. This is to avoid a textual form being altered by the Lisp
;;   reader and then by the Swank processing and then gets misinterpreted when
;;   reaching to Emacs, by wrapping the form inside a string and makes Emacs
;;   evaluate this string.
;; 
;; The following reader macro code is adapted from lisper99's `cltcl' package:
(cl:defun read-white-space (stream)
  "Reads white space from STREAM, including newlines."
  (with-output-to-string (s)
    (loop
       for next = (peek-char nil stream nil)
       while (member next '(#\space #\tab ;; #\newline #\return
                            ))
       do (write-char (read-char stream) s))))

(cl:defun read-comment (stream)
  "Reads a line of lisp comment from STREAM but leaves the newline."
  (with-output-to-string (s)
    (loop for char = (peek-char nil stream)
	      until (member char (list #\return #\newline))
	      do (write-char (read-char stream) s))))

(cl:defun read-double-quoted (stream &optional echo-p)
  "Reads a double quoted expression from STREAM."
  (with-output-to-string (s)
    (loop initially
	 (let ((quote (read-char stream)))
	   (when echo-p (write-char quote s)))
	 for char = (read-char stream)
	 until (eql char #\")
	 do (if (eql char #\\)
		(let ((next-char (read-char stream)))
		  (if echo-p
		      (progn (write-char char s)
			     (write-char next-char s))
		      (write-escaped-char next-char s)))
		(write-char char s))
	 finally (when echo-p (write-char char s)))))

(cl:defun read-word-aux (stream eof-error-p terminator echo-p)
  "Helper for `read-word'."
  (with-output-to-string (s)
    (loop
       for next = (peek-char nil stream eof-error-p)
       until (or (null next)
		 (when terminator
		   (eql next terminator))
		 (member next '(#\space #\tab #\return #\newline #\;)))
       do (let ((char (read-char stream)))
	    (cond ((eql char #\[)
		   (format s "[~{~A~^~%~}]" (read-elisp stream #\])))
		  ((eql char #\\)
		   (let ((next-char (read-char stream)))
		     (if echo-p
			 (progn (write-char char s)
				(write-char next-char s))
			 (write-escaped-char next-char s))))
		  (t (write-char char s)))))))

(cl:defun read-word (stream &optional
		   (eof-error-p t) eof-value recursive-p terminator echo-p)
  "Reads a word from STREAM and returns it as a string. Reads characters untill
the next one is a terminator. The terminator itself is not read. Terminators are
TERMINATOR (when given), whitespace or list terminators. Throws an error if an
end of file occurs, unless EOF-ERROR-P is nil, in which case it returns
EOF-VALUE. If ECHO-P is non nil then escape characters are not handled and the
word is literally copied."
  (read-white-space stream)
  (let ((char (peek-char nil stream eof-error-p recursive-p))) ;; is recursive-p okay?
    (cond ((null char) 
	   eof-value)
	  ((member char (list terminator #\; #\return #\newline))
	   nil)
	  ((eql char #\")
	   (read-double-quoted stream echo-p))
	  ((eql char #\{)
	   (read-braced stream echo-p))
	  (t (read-word-aux stream eof-error-p terminator echo-p)))))

(cl:defun read-elisp-list (&optional (stream *standard-input*) terminator)
  "Reads an Elisp list from STREAM until an end of file occurs or READ-WORD
finds TERMINATOR (when given), a semi-colon or a newline. The list is returned
as a string. The default value for STREAM is *STANDARD-INPUT*. Returns NIL if
end of file is found immediately."
  (read-white-space stream)
  (let ((char (peek-char nil stream nil)))
    (cond
      ((null char) nil)
	  ((eql char #\() (read-elisp-list stream #\))
       ;; don't print out the comment line. All other output lisp expressions
       ;;   will be concatenated into a single line, so if this comment is
       ;;   concatenated into other lisp expression it will comment out the rest
       ;;   of the concatenated line:
       nil 
       )
      ((eql char #\;) (read-comment stream)
       ;; don't print out the comment line. All other output lisp expressions
       ;;   will be concatenated into a single line, so if this comment is
       ;;   concatenated into other lisp expression it will comment out the rest
       ;;   of the concatenated line:
       nil 
       )
	  (t (with-output-to-string (s)
	       (loop
		     for word = (read-word stream t t t terminator t)
		     while word do 
		       (write-string word s)
		       (write-string (read-white-space stream) s)))))))

(cl:defun read-elisp (&optional (stream *standard-input*) terminator)
  "Reads an Elisp script from STREAM until `read-elisp-list'
finds TERMINATOR (when given) or an end of file occurs. Returns the
commands as a list of strings. The default value for STREAM is
*STANDARD-INPUT*."
  (loop 
     for command = (read-elisp-list stream terminator)
     for char = (read-char stream nil)
     if (eql #\; char)
       do (read-comment stream)
     when command collect command
     until (or (null char)
	       (eql char terminator))))

;; ;; BROKEN:

;; ;; This fails to parse the list correctly

;; (cl:set-macro-character #\(
;;                         (lambda (stream char)
;;                           `(cl:format nil "(~{~S~^ ~})" ',(read-elisp stream #\))))
;;                         nil
;;                         (named-readtables:find-readtable :clel))

(cl:set-dispatch-macro-character #\# #\s
   (lambda (stream char i)
     (declare (ignore char i))
     (cl:assert (cl:and (cl:eql (cl:read-char stream) #\t)
		                (cl:eql (cl:read-char stream) #\r)
		                (cl:eql (cl:read-char stream) #\[))
	            ()
	            "Invalid reader macro, use #str[...]")
     `(cl:format nil "~{~A~^ ~}" ',(read-elisp stream #\])))
   (named-readtables:find-readtable :clel))

;; * eval
(cl:defun symbol->string (sym)
  (check-type sym symbol)
  (let ((name (string-downcase (symbol-name sym))))
    ;; touch up on the result
    (cond
      ((keywordp sym)
       (concatenate 'string ":" name))
      ;; `quasiquote' in Lisp means `backquote' in Elisp:
      ((string-equal "quasiquote" name) "backquote")
      (t name))))

(cl:defun form->string (form)
  "Returns a string which emacs will read as equivalent to FORM. FORM can
contain lists, strings, characters, symbols and numbers. This function attempts
to work around the shortcomings of `swank:eval-in-emacs' conversion of FORM,
eg. when evaluating:

  (cl:eval '(list `(,2))) ; <= works fine
  (swank:eval-in-emacs '(list `(,2))) ; <= choked on ,2

The `*package*' is set to the COMMON-LISP package during the translation, and
the readtable the `:clel' one (ie the `invert' readtable case is in effect).

Characters are converted emacs' ?<char> notation, strings are left as they
are (except for espacing any nested \" chars, numbers are printed in base 10 and
symbols are printed as their symbol-name converted to lower case."
  (typecase form
      (string (format nil "~S" form))
      (cons (format nil "(~A . ~A)" ;; "(~A ~{~@[~A~]~^ ~})"
                    (form->string (car form))
                    (form->string (cdr form))
                    ;; (and (cdr form) (form->string (cdr form)))
                    ))
      (character (format nil "?~C" form))
      (symbol (symbol->string form))
      (number (let ((*print-base* 10))
                (princ-to-string form)))
      (t
       (let* ((form-str (prin1-to-string form)))
         ;; form-str length must be greater than 1: if form-str length is zero,
         ;;   then what kind of form is it that `prin1-to-string' produces a
         ;;   null string!? if form-str length is 1, it must be a character or a
         ;;   symbol which should be handled above before reaching here.
         (assert (> (length form-str) 1))
         (if (eql #\, (elt form-str 0))
             (if (not (eql #\( (elt form-str 1)))
                 (string-downcase form-str)
                 (format nil ",~A" (form->string (read-from-string (subseq form-str 1)))))
             (progn
               (warn "~A::form->string(): unhandled case for the form `~A'"
                     'clel ; keep it out of the string for easy symbol
                             ; renaming
                     form-str)
               ;; hack it away to get this done anyway:
               (string-downcase form-str)))))))

(cl:defun eval* (form &optional nowait)
  "Thin wrapper of `swank:eval-in-emacs' or `slynk:eval-in-emacs', which ever
one is available at the moment this function is called.

This function shares the limitations of its wrapped function in that its textual
representation will be altered by the Lisp reader and then by the Swank
processing."
  (funcall (intern "EVAL-IN-EMACS" (or (find-package 'swank)
                                       (find-package 'slynk)))
           form nowait))

(cl:defun eval-string (str &optional str-result nowait)
  "This is less fragile than `eval*' because the form to be evaluated is
wrapped in string and thus not altered by the Lisp reader nor by
the processing of Swank."
  (eval* `(eval (car (read-from-string
                      ,(if str-result
                           (format nil "(format \"%S\" ~S)" str)
                           str))))
         nowait))

(cl:defun eval (form &optional str-result nowait)
  "Convert the FORM to elisp string and send it to Emacs for evaluation. Use
this function for FORM containing at most a *single*
backquote/backtick/quasiquote, otherwise put the form inside a string and use
`eval-string' instead."
  (eval-string (form->string form) str-result nowait))


;; * utils: elisp-lisp hybrid
;;
;; This is for convenient of not having to switch from elisp to lisp via
;; `slime-eval' or `sly-eval'.

;;
;; Note: in CLISP, global variables without the earmuff may get incorrectly
;; eval-ed (source!?). However, keeping this as `current-prefix-arg' to keep the
;; Lisp symbols as close to its Emacs counterparts as possible.
(cl:defvar current-prefix-arg nil
  "Storing the Emacs' `current-prefix-arg' value when the Elisp command is called.")

(cl:defun interactive (&optional form)
  "Emulate the GNU Emacs interactive macro."
  (cond
    ((stringp form)
     (let* ((arglist (ppcre:split "\\n" form)))
       (loop for line in arglist
             for command = (char line 0)
             for prompt = (subseq line 1)
             collect (case command
                       (#\i)
                       (#\s (read-string prompt))
                       (#\P current-prefix-arg)
                       (t (error "IMPLEMENT this case: ~C" command))))))
    ((listp form) form)
    (t (error "Expecting a string or a list, got ~S!"
              (type-of form)))))

(cl:defmacro defun (sym arglist &rest body)
  "Returns a function SYM that eval the BODY. A side effect is that it creates
an Elisp function whose name is <current package>-SYM where <current package> is
the value of *package* at the time of executing this BODY. The Elisp function
passes its prefix argument to the Lisp function SYM."
  ;; test example:
  ;;
  ;; (clel/1::defun a (x) (interactive "P") (format t "current-prefix-arg: ~S~&" x))
  (let* (iform docstr)
    ;; identify the docstr from the BODY argument (if any):
    (when (stringp (car body))
      (setq docstr (car body)
            body (cdr body)))
    ;; identify the interactive form from the (remaining) BODY argument, after
    ;; it has been parsed for the docstring:
    (let* ((form (car body)))
      (when (and (consp form)
                 ;; (eq 'interactive (car form))
                 (string-equal ; case-insensitive string comparison
                  (format nil "~(~A~)" (symbol-name (car form)))
                  "interactive"))
        (setq iform (cons 'interactive (cdr form))
              body (cdr body))
        (assert (or (listp iform) (stringp iform)))))
    ;; now make an Elisp wrapper of this CL function:

    ;; Create the Elisp command that calls this function CLSYM:
    ;;
    ;; DESIGN:
    ;;   wrap things in string to avoid the Lisp reader removing the package
    ;;   prefix and making symbol upcase and what not as well as to evade the
    ;;   Swank processing of form into string which alters the original
    ;;   textual presentation of the form.
    ;;
    ;;   example: (clel/1:defun a (x) (interactive "P") (format t "~S~&" x))
    ;;     this will create a function `clel-a' on the Emacs side and the
    ;;     function `a' on the Lisp side, the package of which is the value of
    ;;     cl:*package* at the time this macro (`defun') is called.
    (eval-string (format nil
                         "(defun ~(~A-~A~) (prefix)
          (interactive \"P\")
          (let* ((str (format ~S prefix)))
          (clel-slime-eval-string str)))"
                         'clel sym
                         (format nil
                                 "(cl:let* ((cl:*package* ~(~A~)::~(~A~))
              (~A::~A '%S))
              (cl:destructuring-bind ~S ~S
                (~(~A~) ~{~S~^ ~})))"
                                 'clel/1 'current-prefix-arg
                                 ;; cl:let* ((cl:*package* ...))
                                 'clel/1 '*clel-package*
                                 ;; destructuring bind:
                                 arglist iform
                                 sym arglist)))
    `(cl:defun ,sym ,arglist ,docstr ,@body)))

