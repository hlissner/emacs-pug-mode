;;; jaded-mode.el --- Major mode for jade template files
;;
;; Copyright (c) 2007, 2008 Nathan Weizenbaum
;; Copyright (c) 2009-2013 Daniel Mendler
;; Copyright (c) 2012-2014 Bozhidar Batsov
;; Copyright (c) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner
;; Inspired by: http://github.com/slim-template/emacs-slim
;; URL: http://github.com/hlissner/jaded-mode
;; Version: 1.0
;; Keywords: markup, language, jade
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (require 'cl))
(require 'js)

;; User definable variables

(defgroup jaded nil
  "Support for the Jade template language."
  :group 'languages
  :prefix "jaded-")

(defcustom jaded-mode-hook nil
  "Hook run when entering Jade mode."
  :type 'hook
  :group 'jaded)

(defcustom jaded-backspace-backdents-nesting t
  "Non-nil to have `jaded-electric-backspace' re-indent all code
nested beneath the backspaced line be re-indented along with the
line itself."
  :type 'boolean
  :group 'jaded)

(defvar jaded-indent-function 'jaded-indent-p
  "This function should look at the current line and return true
if the next line could be nested within this line.")


;; Font lock

(defconst jaded-tags-re
  (concat "^ *\\("
          (regexp-opt
           '("a" "abbr" "acronym" "address" "applet" "area" "article" "aside"
             "audio" "b" "base" "basefont" "bdo" "big" "blockquote" "body"
             "br" "button" "canvas" "caption" "center" "cite" "code" "col"
             "colgroup" "command" "datalist" "dd" "del" "details" "dialog" "dfn"
             "dir" "div" "dl" "dt" "em" "embed" "fieldset" "figure" "font" "footer"
             "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6"
             "head" "header" "hgroup" "hr" "html" "i"
             "iframe" "img" "input" "ins" "keygen" "kbd" "label" "legend" "li" "link"
             "map" "mark" "menu" "meta" "meter" "nav" "noframes" "noscript" "object"
             "ol" "optgroup" "option" "output" "p" "param" "pre" "progress" "q" "rp"
             "rt" "ruby" "s" "samp" "script" "section" "select" "small" "source" "span"
             "strike" "strong" "style" "sub" "sup" "table" "tbody" "td" "textarea" "tfoot"
             "th" "thead" "time" "title" "tr" "tt" "u" "ul" "var" "video" "xmp") 'words)
          "\\)")
  "Regex of all html4/5 tags.")

(defconst jaded-selfclosing-tags-re
  (concat "^ *"
          (regexp-opt
           '("meta" "title" "img" "area" "base" "br" "col" "command" "embed" "hr" "input"
             "link" "param" "source" "track" "wbr") t)))

(defconst jaded-keywords-re
  (concat "^ *\\(?:- \\)?" (regexp-opt '("extends" "block") t)))

(defconst jaded-control-re
  (concat "^ *\\(- \\)?\\("
          (regexp-opt
           '("if" "unless" "while" "until" "else" "for"
             "begin" "elsif" "when" "default" "case" "var'"

             "extends" "block" "mixin"
             ) 'words)
          "\\)"))

;; Helper for nested block (comment, embedded, text)
(defun jaded-nested-re (re)
  (concat "^\\( *\\)" re "\n\\(\\(?:\\1" (make-string tab-width ? ) ".*\\| *\\)\n\\)*"))

(defconst jaded-font-lock-keywords
  `(;; comment block
    (,(jaded-nested-re "//")
     0 font-lock-comment-face)
    ;; comment line
    ("^ *\\(-//\\|//-?\\).*"
     0 font-lock-comment-face prepend)
    ;; html comment block
    ("<!--.*-->"
     0 font-lock-comment-face)
    ;; filters
    (,(jaded-nested-re "\\(:[a-z0-9_]+\\)")
     (0 font-lock-preprocessor-face prepend))
    ;; text block
    ;; (,(jaded-nested-re "[\\.#+a-z][^ \t]*\\(?:(.+)\\)?\\(\\.\\)")
    ;;  2 font-lock-string-face)

    (,jaded-control-re
     2 font-lock-keyword-face append)

    ("^ *|.*"
     0 font-lock-string-face)

    ;; Single quote string
    ("[^a-z]\\('[^'\n]*'\\)"
     1 font-lock-string-face append)
    ;; Double quoted string
    ("\\(\"[^\"]*\"\\)"
     1 font-lock-string-face append)

    ;; interpolation
    ("[#!]{.+}" . font-lock-constant-face)

    ("^ *\\(include\\)\\(:[^ \t]+\\|\\)\\(.+\\)\n"
     (1 font-lock-keyword-face)
     (2 font-lock-preprocessor-face)
     (3 font-lock-string-face))

    ;; +mixin invocation
    ("^ *\\+\\([a-z0-9_-]+\\)"
     0 font-lock-builtin-face)
    ;; #id
    ("^ *[a-z0-9_.-]*\\(#[a-z0-9_-]+\\((.*)\\)?\\)[^ \t]*$"
     1 font-lock-keyword-face append)
    ;; .class
    ("^ *[a-z0-9_#-]*\\(\\(\\.[a-z0-9_-]+\\((.*)\\)?\\)+\\)[^ \t]*$"
     1 font-lock-variable-name-face append)
    ;; tag
    (,jaded-tags-re
     1 font-lock-function-name-face)

    ;; doctype
    ("^\\(doctype .*$\\)"
     1 font-lock-comment-face)
    ;; ==', =', -
    ("^ *\\(==?'?\\|-\\)"
      (1 font-lock-preprocessor-face)
      (,(regexp-opt
         '("if" "else" "elsif" "for" "in" "do" "unless"
           "while" "yield" "not" "and" "or")
         'words) nil nil
           (0 font-lock-keyword-face)))
    ;; tag ==, tag =
    ("^ *[\\.#a-z0-9_-]+.*[^<>!]\\(==?'?\\) +"
     1 font-lock-preprocessor-face)
    ))

(defconst jaded-embedded-re "^ *:[a-z0-9_-]+")
(defconst jaded-plain-re "^ *[\\.#+a-z][^ \t]*\\(?:(.+)\\)?\\.")
(defconst jaded-comment-re "^ *-?//-?")

(defun* jaded-extend-region ()
  "Extend the font-lock region to encompass embedded engines and comments."
  (let ((old-beg font-lock-beg)
        (old-end font-lock-end))
    (save-excursion
      (goto-char font-lock-beg)
      (unless (looking-at "\\.$")
        (beginning-of-line)
        (unless (or (looking-at jaded-embedded-re)
                    (looking-at jaded-comment-re))
          (return-from jaded-extend-region)))
      (setq font-lock-beg (point))
      (jaded-forward-sexp)
      (beginning-of-line)
      (setq font-lock-end (max font-lock-end (point))))
    (or (/= old-beg font-lock-beg)
        (/= old-end font-lock-end))))

(defvar jade-tag-declaration-char-re "[-a-zA-Z0-9_.#+]"
  "Regexp used to match a character in a tag declaration")

(defun jade-goto-end-of-tag ()
  "Skip ahead over whitespace, tag characters (defined in
`jade-tag-declaration-char-re'), and paren blocks (using
`forward-sexp') to put point at the end of a full tag declaration (but
before its content). Use when point is inside or to the left of a tag
declaration"
  (interactive)

  ;; skip indentation characters
  (while (looking-at "[ \t]")
    (forward-char 1))

  (while (looking-at jade-tag-declaration-char-re)
    (forward-char 1))
  (if (looking-at "(")
      (forward-sexp 1)))

;; Mode setup

(defvar jaded-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in jaded-mode buffers.")

(defvar jaded-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\177" 'jaded-electric-backspace)
    (define-key map "\C-?" 'jaded-electric-backspace)
    (define-key map "\C-c\C-f" 'jaded-forward-sexp)
    (define-key map "\C-c\C-b" 'jaded-backward-sexp)
    (define-key map "\C-c\C-u" 'jaded-up-list)
    (define-key map "\C-c\C-d" 'jaded-down-list)
    (define-key map "\C-c\C-k" 'jaded-kill-line-and-indent)
    map))

;; For compatibility with Emacs < 24, derive conditionally
(defalias 'jaded-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode jaded-mode jaded-parent-mode "Jaded"
  "Major mode for editing Jade files.

\\{jaded-mode-map}"
  (set-syntax-table jaded-mode-syntax-table)
  (add-to-list 'font-lock-extend-region-functions 'jaded-extend-region)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'indent-line-function) 'jaded-indent-line)
  (set (make-local-variable 'indent-region-function) 'jaded-indent-region)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'electric-indent-chars) nil)
  (setq comment-start "/")
  (setq indent-tabs-mode nil)
  (setq font-lock-defaults '((jaded-font-lock-keywords) nil t)))

;; Useful functions

(defun jaded-comment-block ()
  "Comment the current block of Jade code."
  (interactive)
  (save-excursion
    (let ((indent (current-indentation)))
      (back-to-indentation)
      (insert "/")
      (newline)
      (indent-to indent)
      (beginning-of-line)
      (jaded-mark-sexp)
      (jaded-reindent-region-by tab-width))))

(defun jaded-uncomment-block ()
  "Uncomment the current block of Jade code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (not (looking-at jaded-comment-re))
      (jaded-up-list)
      (beginning-of-line))
    (jaded-mark-sexp)
    (kill-line 1)
    (jaded-reindent-region-by (- tab-width))))

;; Navigation

(defun jaded-forward-through-whitespace (&optional backward)
  "Move the point forward at least one line, until it reaches
either the end of the buffer or a line with no whitespace.

If `backward' is non-nil, move the point backward instead."
  (let ((arg (if backward -1 1))
        (endp (if backward 'bobp 'eobp)))
    (loop do (forward-line arg)
          while (and (not (funcall endp))
                     (looking-at "^[ \t]*$")))))

(defun jaded-at-indent-p ()
  "Returns whether or not the point is at the first
non-whitespace character in a line or whitespace preceding that
character."
  (let ((opoint (point)))
    (save-excursion
      (back-to-indentation)
      (>= (point) opoint))))

(defun jaded-forward-sexp (&optional arg)
  "Move forward across one nested expression.
With `arg', do it that many times.  Negative arg -N means move
backward across N balanced expressions.

A sexp in Jade is defined as a line of Jade code as well as any
lines nested beneath it."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (< arg 0) (not (jaded-at-indent-p)))
      (back-to-indentation)
    (while (/= arg 0)
      (let ((indent (current-indentation)))
        (loop do (jaded-forward-through-whitespace (< arg 0))
              while (and (not (eobp))
                         (not (bobp))
                         (> (current-indentation) indent)))
        (back-to-indentation)
        (setq arg (+ arg (if (> arg 0) -1 1)))))))

(defun jaded-backward-sexp (&optional arg)
  "Move backward across one nested expression.
With ARG, do it that many times.  Negative arg -N means move
forward across N balanced expressions.

A sexp in Jade is defined as a line of Jade code as well as any
lines nested beneath it."
  (interactive "p")
  (jaded-forward-sexp (if arg (- arg) -1)))

(defun jaded-up-list (&optional arg)
  "Move out of one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (loop do (jaded-forward-through-whitespace t)
            while (and (not (bobp))
                       (>= (current-indentation) indent)))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun jaded-down-list (&optional arg)
  "Move down one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (jaded-forward-through-whitespace)
      (when (<= (current-indentation) indent)
        (jaded-forward-through-whitespace t)
        (back-to-indentation)
        (error "Nothing is nested beneath this line"))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun jaded-mark-sexp ()
  "Marks the next Jade block."
  (let ((forward-sexp-function 'jaded-forward-sexp))
    (mark-sexp)))

(defun jaded-mark-sexp-but-not-next-line ()
  "Marks the next Jade block, but puts the mark at the end of the
last line of the sexp rather than the first non-whitespace
character of the next line."
  (jaded-mark-sexp)
  (let ((pos-of-end-of-line (save-excursion
                              (goto-char (mark))
                              (end-of-line)
                              (point))))
    (when (/= pos-of-end-of-line (mark))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (forward-line -1)
         (end-of-line)
         (point))))))

;; Indentation and electric keys

(defun jaded-indent-p ()
  "Returns true if the current line can have lines nested beneath it."
  (or (looking-at jaded-comment-re)
      (looking-at jaded-embedded-re)
      (and (save-excursion
             (back-to-indentation)
             (not (memq (face-at-point) '(font-lock-preprocessor-face))))
           (not (looking-at jaded-selfclosing-tags-re))
           (loop for opener in `(,(concat "^ *\\([\\.#+]\\|" jaded-tags-re "\\)[^ \t]*\\((.+)\\)?\n")
                                 "^ *[\\.#+a-z][^ \t]*\\(?:(.+)\\)?\\.\n"
                                 "^ *[-=].*do[ \t]*\\(|.*|[ \t]*\\)?$"
                                 ,jaded-control-re)
                 if (looking-at opener) return t
                 finally return nil))))

(defun jaded-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) 0
      (jaded-forward-through-whitespace t)
      (+ (current-indentation)
         (if (funcall jaded-indent-function)
             tab-width
           0)))))

(defun jaded-indent-region (start end)
  "Indent each nonblank line in the region.
This is done by indenting the first line based on
`jaded-compute-indentation' and preserving the relative
indentation of the rest of the region.

If this command is used multiple times in a row, it will cycle
between possible indentations."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (let (this-line-column current-column
          (next-line-column
           (if (and (equal last-command this-command) (/= (current-indentation) 0))
               (* (/ (- (current-indentation) 1) tab-width) tab-width)
             (jaded-compute-indentation))))
      (while (< (point) end)
        (setq this-line-column next-line-column
              current-column (current-indentation))
        ;; Delete whitespace chars at beginning of line
        (delete-horizontal-space)
        (unless (eolp)
          (setq next-line-column (save-excursion
                                   (loop do (forward-line 1)
                                         while (and (not (eobp)) (looking-at "^[ \t]*$")))
                                   (+ this-line-column
                                      (- (current-indentation) current-column))))
          ;; Don't indent an empty line
          (unless (eolp) (indent-to this-line-column)))
        (forward-line 1)))
    (move-marker end nil)))

(defun jaded-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `tab-width' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (jaded-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) tab-width) tab-width))
        (indent-to need)))
      (if (< (current-column) (current-indentation))
          (forward-to-indentation 0))))

(defun jaded-reindent-region-by (n)
  "Add N spaces to the beginning of each line in the region.
If N is negative, will remove the spaces instead.  Assumes all
lines in the region have indentation >= that of the first line."
  (let ((ci (current-indentation))
        (bound (mark)))
    (save-excursion
      (while (re-search-forward (concat "^" (make-string ci ? )) bound t)
        (replace-match (make-string (max 0 (+ ci n)) ? ) bound nil)))))

(defun jaded-electric-backspace (arg)
  "Delete characters or back-dent the current line.
If invoked following only whitespace on a line, will back-dent
the line and all nested lines to the immediately previous
multiple of `tab-width' spaces.

Set `jaded-backspace-backdents-nesting' to nil to just back-dent
the current line."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (looking-at "^[ \t]+$"))
      (backward-delete-char-untabify arg)
    (save-excursion
      (let ((ci (current-column)))
        (beginning-of-line)
        (if jaded-backspace-backdents-nesting
            (jaded-mark-sexp-but-not-next-line)
          (set-mark (save-excursion (end-of-line) (point))))
        (jaded-reindent-region-by (* (- arg) tab-width))
        (back-to-indentation)
        (pop-mark)))))

(defun jaded-kill-line-and-indent ()
  "Kill the current line, and re-indent all lines nested beneath it."
  (interactive)
  (beginning-of-line)
  (jaded-mark-sexp-but-not-next-line)
  (kill-line 1)
  (jaded-reindent-region-by (* -1 tab-width)))

(defun jaded-indent-string ()
  "Return the indentation string for `tab-width'."
  (mapconcat 'identity (make-list tab-width " ") ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jaded-mode))

;; Setup/Activation
(provide 'jaded-mode)
;;; jaded-mode.el ends here
