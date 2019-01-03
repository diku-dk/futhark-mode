;;; futhark-highlight.el --- highlighting for futhark-mode  -*- lexical-binding: t; -*-

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

(defun futhark-highlight-syms-re (syms)
  "Create a regular expression matching any of the symbols in SYMS.
This is useful because the normal word boundaries are not what we
want (for example, we consider underscore to be part of a symbol
name)."
  (concat "\\_<" (regexp-opt syms t) "\\_>"))

(defvar futhark-highlight-font-lock
  `(

    ;; Variable and tuple declarations.
      ;;; Lets.
      ;;;; Primitive values.
    (,(concat "let" futhark-const-ws1
              "\\(" futhark-const-var "\\)")
     . '(1 font-lock-variable-name-face))
      ;;;; Tuples.  XXX: It would be nice to highlight only the variable names
      ;;;; inside the parantheses, and not also the commas.
    (,(concat "let" futhark-const-ws1 "("
              "\\(" "[^)]+" "\\)")
     . '(1 font-lock-variable-name-face))
      ;;; Function parameters.
    (,(concat "\\(" futhark-const-var "\\)" futhark-const-ws ":")
     . '(1 font-lock-variable-name-face))

    ;; Constants.
      ;;; Booleans.
    (,(futhark-highlight-syms-re futhark-const-booleans)
     . font-lock-constant-face)

      ;;; Numbers
    (,(concat "\\(" futhark-const-number "\\)")
     . font-lock-constant-face)

      ;;; Characters
    (,(concat "\\(" futhark-const-character "\\)")
     . font-lock-constant-face)

      ;;; Constructors
    (,(concat "\\(" futhark-const-constructor "\\)")
     . font-lock-constant-face)

    ;; Keywords.
    ;; Placed after constants, so e.g. '#open' is highlighted
    ;; as a value and not as a keyword.
    (,(futhark-highlight-syms-re futhark-const-keywords)
     . font-lock-keyword-face)

    ;; Types.
      ;;; Type aliases.  XXX: It would be nice to highlight also the right hand
      ;;; side.
    (,(concat "type" futhark-const-ws1 "\\(" futhark-const-type "\\)")
     . '(1 font-lock-type-face))
      ;;; Function parameters types and return type.
    (,(concat ":" futhark-const-ws "\\(" "[^=,)]+" "\\)")
     . '(1 font-lock-type-face))
      ;;; Builtin types.
    (,(futhark-highlight-syms-re futhark-const-builtin-types)
     . font-lock-type-face)

    ;; Builtins.
      ;;; Functions.
      ;;;; Builtin functions.
    (,(futhark-highlight-syms-re futhark-const-builtin-functions)
     . font-lock-builtin-face)
      ;;; Operators.
    (,futhark-const-operator
     . font-lock-builtin-face)
    )
  "Highlighting expressions for Futhark.")

(defvar futhark-highlight-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n ">" st)
    ;; Make apostrophe and underscore be part of variable names.
    ;; Technically, they should probably be part of the symbol class,
    ;; but it works out better for some of the regexpes if they are part
    ;; of the word class.
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[  "(]" st)
    (modify-syntax-entry ?\]  ")[" st)
    (modify-syntax-entry ?\{  "(}" st)
    (modify-syntax-entry ?\}  "){" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "_" st)

    (modify-syntax-entry ?# "'!~" st)

    ;; Symbol characters are treated as punctuation because they are
    ;; not able to form identifiers with word constituent 'w' class.
    ;; The '-' symbol is handled specially because it is also used for
    ;; line comments.
    (mapc (lambda (x)
            (modify-syntax-entry x "." st))
          "+*/%=!><|&^")

    (mapc (lambda (c) (modify-syntax-entry c "_" st)) "._'\\")

    (mapc (lambda (x)
            (modify-syntax-entry x "." st))
          ",:")

    ;; Define the -- line comment syntax.
    (modify-syntax-entry ?- ". 123" st)
    st)
  "Syntax table used in `futhark-mode'.")

(provide 'futhark-highlight)

;;; futhark-highlight.el ends here
