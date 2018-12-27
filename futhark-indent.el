;;; futhark-indent.el --- indentation for futhark-mode  -*- lexical-binding: t; -*-

;; Copyright (C) DIKU 2013-2019, University of Copenhagen
;;
;; URL: https://github.com/diku-dk/futhark-mode
;; Keywords: languages
;; Version: 0.2
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; License:
;; ICS <https://github.com/diku-dk/futhark-mode/blob/master/LICENSE>

;;; Commentary:
;; See futhark-mode.el.

;;; Code:

(require 'futhark-const)

(defvar futhark-indent-level 2
  "The basic indent level for `futhark-mode'.")

(defun futhark-indent-line ()
  "Indent current line as Futhark code."
  (let ((savep (> (current-column) (current-indentation)))
        (indent (or (futhark-indent-calculate-indentation)
                    (current-indentation))))
    (if savep ; The cursor is beyond leading whitespace.
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun futhark-indent-calculate-indentation ()
  "Calculate the indentation for the current line.
In general, prefer as little indentation as possible."
  (let ((parse-sexp-lookup-properties t)
        (parse-sexp-ignore-comments t))

    (save-excursion
      (futhark-indent-beginning-of-line-text)

      ;; The following code is fickle and deceptive.  Don't change it
      ;; unless you kind of know what you're doing!
      (or

       ;; Align comment to next non-comment line.
       (and (looking-at comment-start)
            (forward-comment (count-lines (point-min) (point)))
            (current-column))

       ;; Align global definitions and headers to nearest module definition or
       ;; column 0.
       ;;
       ;; Detecting whether a 'let' is top-level or local is really
       ;; hard.  We embed the heuristic that if the previous line is
       ;; blank, then it is top-level.
       (and (or (futhark-indent-looking-at-word "entry")
                (futhark-indent-looking-at-word "type")
                (futhark-indent-looking-at-word "val")
                (futhark-indent-looking-at-word "module")
                (futhark-indent-looking-at-word "local")
                (futhark-indent-looking-at-word "include")
                (futhark-indent-looking-at-word "import")
                (and (futhark-indent-looking-at-word "let")
                     (save-excursion
                       (forward-line -1)
                       (futhark-indent-is-empty-line))))
            (or
             (save-excursion
               (and
                (ignore-errors (backward-up-list 1) t)
                (looking-at "{")
                (or
                 (futhark-indent-keyword-backward "local")
                 (futhark-indent-keyword-backward "module")
                 (futhark-indent-keyword-backward "open")
                 t)
                (+ futhark-indent-level (current-column))))
             0))

       ;; Align closing curly brackets to the matching opening 'module'
       ;; keyword.
       (save-excursion
         (and (looking-at "}")
              (ignore-errors
                (backward-up-list 1)
                (or
                 (save-excursion
                   (ignore-errors
                     (and
                      (backward-up-list 1)
                      (looking-at "(")
                      (futhark-indent-keyword-backward "module\\|open")
                      (current-column))))
                 (and
                  (futhark-indent-keyword-backward "module\\|open")
                  (current-column))))))

       ;; Align closing parentheses and commas to the matching opening
       ;; parenthesis.
       (save-excursion
         (and (looking-at (regexp-opt '(")" "]" "}" ",")))
              (ignore-errors
                (backward-up-list 1)
                (current-column))))

       ;; Align additional constructors in a type abbreviation to the
       ;; '=' sign.
       (save-excursion
         (and (save-excursion
                (looking-at "|")
                (futhark-indent-keyword-backward "let\\|type")
                (looking-at "type"))
              (save-excursion
               (futhark-indent-symbol-backward "=")
               (current-column))))

       ;; If the previous code line ends with "=" or "->", align to
       ;; the matching "let", "entry", "loop", "case", or "\" column
       ;; plus one indent level.
       (save-excursion
         (and (futhark-indent-backward-part)
              (futhark-indent-forward-part)
              (looking-at "[[:space:]]*\\(?:=\\|->\\)[[:space:]]*$")
              (let ((m
                     (futhark-indent-max
                      (save-excursion
                        (futhark-indent-keyword-backward "let"))
                      (save-excursion
                        (futhark-indent-keyword-backward "entry"))
                      (save-excursion
                        (futhark-indent-keyword-backward "loop"))
                      (save-excursion
                        (futhark-indent-keyword-backward "case"))
                      (save-excursion
                        (futhark-indent-symbol-backward "\\\\")))))
                (and (not (eq nil m))
                     (goto-char m)
                     (+ (current-column) futhark-indent-level)))))

       ;; Align "in", "let", or "loop" to the closest previous "let" or "loop".
       (save-excursion
         (and (or (futhark-indent-looking-at-word "in")
                  (futhark-indent-looking-at-word "let")
                  (futhark-indent-looking-at-word "loop"))
              (let ((m
                     (futhark-indent-max
                      (save-excursion
                        (futhark-indent-backward-part)
                        (cond ((looking-at "unsafe")
                               (point))
                              ((and (looking-at "else")
                                    (futhark-indent-find-principal-if))
                               (point))))
                      (save-excursion
                        ;; Careful that we are not confused by a nested 'let'.
                        (let ((m2 (futhark-indent-keyword-backward "let\\|in")))
                          (when (looking-at "let")
                            m2)))
                      (save-excursion
                        (futhark-indent-backward-part)
                        (when (looking-at "do")
                          (futhark-indent-keyword-backward "loop"))))))
                (and (not (eq nil m))
                     (goto-char m)
                     (current-column)))))

       ;; Otherwise, if the previous code line ends with "in" align to
       ;; the matching "let" or "loop" column.
       (save-excursion
         (and (futhark-indent-backward-part)
              (looking-at "\\<in[[:space:]]*$")
              (let ((m
                     (futhark-indent-max
                      (save-excursion
                        (futhark-indent-keyword-backward "let"))
                      (save-excursion
                        (futhark-indent-keyword-backward "loop")))))
                (and (not (eq nil m))
                     (goto-char m)
                     (current-column)))))

       ;; Align "case" to nearest "match" or "case".  Note that
       ;; indenting "match" itself is handled by the usual rules;
       ;; there is nothing special about it.
       (save-excursion
         (and (futhark-indent-looking-at-word "case")
              (futhark-indent-keyword-backward "case\\|match")
              (or
               (let ((curline (line-number-at-pos)))
                 (save-excursion
                   (and (futhark-indent-backward-part)
                        (= (line-number-at-pos) curline)
                        (futhark-indent-looking-at-word "case\\|match")
                        (current-column))))
               (current-column))))

       ;; Align "else" to nearest "else if".  (this is to
       ;; make if-then-else chains nicer).
       (save-excursion
         (and (looking-at "else\\|then")
              (futhark-indent-find-principal-if)
              ;; If the "if" immediately follows an "else", then align
              ;; to that "else" instead (this is to make if-then-else
              ;; chains nicer).
              (futhark-indent-backward-part)
              (futhark-indent-looking-at-word "else")
              (current-column)))

       ;; Align "else/then" to nearest "then" or "else if" or "if".
       (save-excursion
         (and (futhark-indent-looking-at-word "else\\|then")
              (futhark-indent-find-principal-if)
              (current-column)))

       ;; An 'if' following an 'else' gets aligned to the corresponding preceding 'if'.
       (save-excursion
         (and (futhark-indent-looking-at-word "if")
              (futhark-indent-backward-part)
              (futhark-indent-looking-at-word "else")
              (futhark-indent-find-principal-if)
              (current-column)))

       ;; Align an operator to the column of the first token on the
       ;; previous line.
       (save-excursion
         (and (looking-at futhark-const-operator)
              (futhark-indent-find-function)
              (current-column)))

       ;; Align a function argument to the column of the first
       ;; argument to the function.
       (save-excursion
         (let ((m (point))
               (f (and (not (futhark-indent-is-looking-at-keyword))
                       (not (looking-at "[]})]"))
                       (futhark-indent-find-function)
                       (not (futhark-indent-is-looking-at-keyword)))))
           (when (and f (/= (point) m))
             (or (save-excursion
                  (and (futhark-indent-forward-part)
                       (futhark-indent-forward-part)
                       (futhark-indent-backward-part)
                       (/= m (point))
                       (current-column)))
                 (current-column)))))

       ;; Align something following 'else' to the corresponding 'if'.
       (save-excursion
         (and (futhark-indent-backward-part)
              (futhark-indent-looking-at-word "else")
              (futhark-indent-find-principal-if)
              (current-column)))

       ;; Align general content inside parentheses to the first general
       ;; non-space content.
       (save-excursion
         (when (ignore-errors (backward-up-list 1) t)
              (forward-char 1)
              (futhark-indent-goto-first-text)
              (and
               (not (futhark-indent-is-looking-at-keyword))
               (not (looking-at "\\\\")) ; is not a lambda.
               (current-column))))

       ;; Otherwise, keep the user-specified indentation level.
       ))))

(defun futhark-indent-min (&rest args)
  "Like `min', but also accepts nil values in ARGS."
  (let ((args-nonnil (cl-remove-if-not 'identity args)))
    (if args-nonnil
        (apply 'min args-nonnil)
      nil)))

(defun futhark-indent-max (&rest args)
  "Like `max', but also accepts nil values in ARGS."
  (let ((args-nonnil (cl-remove-if-not 'identity args)))
    (if args-nonnil
        (apply 'max args-nonnil)
      nil)))

(defun futhark-indent-beginning-of-line-text ()
  "Move to the beginning of the non-whitespace text on this line."
  (beginning-of-line)
  (futhark-indent-goto-first-text))

(defun futhark-indent-goto-first-text ()
  "Skip over whitespace."
  (while (looking-at "[[:space:]\n]")
    (forward-char)))

(defun futhark-indent-is-beginning-of-line-text ()
  "Check if point is at the first word on a line."
  (=
   (point)
   (save-excursion
     (futhark-indent-beginning-of-line-text)
     (point))))

(defun futhark-indent-is-empty-line ()
  "Check if the line of the current point is empty.
It is considered empty if the line consists of zero or more
whitespace characters."
  (let ((cur (line-number-at-pos)))
    (futhark-indent-beginning-of-line-text)
    (not (= cur (line-number-at-pos)))))

(defun futhark-indent-is-looking-at-keyword ()
  "Check if we are currently looking at a keyword."
  (cl-some 'futhark-indent-looking-at-word futhark-const-keywords))

(defun futhark-indent-backward-part ()
  "Try to jump back one sexp.
The net effect seems to be that it works ok."
  (and (not (bobp))
       (ignore-errors (backward-sexp 1) t)))

(defun futhark-indent-forward-part ()
  "Try to jump forward one sexp.
The net effect seems to be that it works ok."
  (and (not (eobp))
       (ignore-errors (forward-sexp 1) t)))

(defun futhark-indent-looking-at-word (word)
  "Do the same as `looking-at', but also check for blanks around WORD."
  (looking-at (concat "\\<" word "\\>")))

(defun futhark-indent-back-actual-line ()
  "Go back to the first non-empty line, or return nil trying."
  (let (bound)
    (while (and (not (bobp))
                (forward-line -1)
                (progn (beginning-of-line)
                       (setq bound (point))
                       (end-of-line)
                       t)
                (ignore-errors
                  (re-search-backward "^[[:space:]]*$" bound))))))

(defun futhark-indent-something-backward (check)
  "Do something backwards using CHECK."
  ;; FIXME: Support nested let-chains.  This used to work, but was removed
  ;; because the code was too messy.
  (let (;; Only look in the current paren-delimited code if present.
        (startp (point))
        (topp (or (save-excursion (ignore-errors
                                    (backward-up-list 1)
                                    (point)))
                  (max
                   (or (save-excursion (futhark-indent-keyword-backward-raw "let"))
                       0)
                   (or (save-excursion (futhark-indent-keyword-backward-raw "entry"))
                       0))))
        (result nil))

    (while (and (not result)
                (futhark-indent-backward-part)
                (>= (point) topp))

      (if (funcall check)
          (setq result (point))))

    (or result
        (progn
          (goto-char startp)
          nil))))

(defun futhark-indent-keyword-backward (word)
  "Go to a keyword WORD before the current position.
Set mark and return t if found; return nil otherwise."
  (futhark-indent-something-backward (lambda () (futhark-indent-looking-at-word word))))

(defun futhark-indent-symbol-backward (symbol)
  "Go to a symbol SYMBOL before the current position.
Set mark and return t if found; return nil otherwise."
  (futhark-indent-something-backward (lambda () (looking-at symbol))))

(defun futhark-indent-keyword-backward-raw (word)
  "Go to a keyword WORD before the current position.
Ignore any program structure."
  (while (and (futhark-indent-backward-part)
              (not (futhark-indent-looking-at-word word))))
  (and (futhark-indent-looking-at-word word)
       (point)))

(defun futhark-indent-find-function ()
  "Find the start of the function being applied in the current expression, if any."
  ;; This is pretty hacky, but it seems to work OK.
  (or
   (and (futhark-indent-something-backward
         (lambda () (or (futhark-indent-looking-at-word "do\\|in")
                        (save-excursion
                          (and (futhark-indent-forward-part)
                               (looking-at (concat "[[:space:]]*\\(?:,\\|->\\|" futhark-const-operator "\\)"))))
                        (looking-at "=\\|->\\|,"))))
        ;; Go to just after the separator.
        (futhark-indent-forward-part)
        ;; Go to just after the function.
        (futhark-indent-forward-part)
        ;; Go to just before the function.
        (futhark-indent-backward-part))
   (when (ignore-errors (backward-up-list 1) t)
     (forward-char 1)
     (futhark-indent-goto-first-text)
     t)))

(defun futhark-indent-find-principal-if ()
  "Find the 'if' keyword controlling the current 'else' or 'then'."
  (futhark-indent-keyword-backward "else\\|if")
  (cond ((looking-at "else")
         (futhark-indent-find-principal-if)
         (futhark-indent-keyword-backward "if"))
        ((looking-at "if")
         t)))

(provide 'futhark-indent)

;;; futhark-indent.el ends here
