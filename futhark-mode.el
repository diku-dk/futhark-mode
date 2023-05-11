;;; futhark-mode.el --- major mode for editing Futhark source files  -*- lexical-binding: t; -*-

;; Copyright (C) DIKU 2013-2019, University of Copenhagen
;;
;; URL: https://github.com/diku-dk/futhark-mode
;; Keywords: languages
;; Version: 0.2
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;; License:
;; ICS <https://github.com/diku-dk/futhark-mode/blob/master/LICENSE>

;;; Commentary:
;; Futhark is a small programming language designed to be compiled to efficient
;; GPU code.  This Emacs mode provides syntax highlighting, conservative
;; automatic indentation, interpreter interaction.
;;
;; Files with the ".fut" extension are automatically handled by this mode.
;;
;; For extensions: Define local keybindings in `futhark-mode-map'.  Add startup
;; functions to `futhark-mode-hook'.

;;; Code:

(require 'cl-lib)
(require 'futhark-highlight)
(require 'futhark-indent)
(require 'futhark-comint)

(defgroup futhark ()
  "Editing Futhark source files."
  :group 'languages)

(defvar futhark-mode-hook nil
  "Hook for `futhark-mode'.  Is run whenever the mode is entered.")

(defvar futhark-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-c\C-l" #'futhark-comint-load-file)
    map)
  "Keymap for `futhark-mode'.")

(defvar futhark-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?' "_" st)
    (modify-syntax-entry ?# "_" st)

    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\` "$`" st)

    ;; Symbol characters are treated as punctuation because they are
    ;; not able to form identifiers with word constituent 'w' class.
    ;; The '-' symbol is handled specially because it is also used for
    ;; line comments.
    (mapc (lambda (x)
            (modify-syntax-entry x "." st))
          "+*/%=!><|&^\\")

    (mapc (lambda (c) (modify-syntax-entry c "_" st)) "._")

    (mapc (lambda (x)
            (modify-syntax-entry x "." st))
          ",:")

    ;; Define the -- line comment syntax.
    (modify-syntax-entry ?- ". 123" st)
    st)
  "Syntax table used in `futhark-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fut\\'" . futhark-mode))

;;;###autoload
(define-derived-mode futhark-mode prog-mode "Futhark"
  "Major mode for editing Futhark source files."
  :syntax-table futhark-syntax-table
  (setq-local comment-start "--")
  (setq-local comment-start-skip "--[ \t]*")
  (setq-local paragraph-start (concat " *-- |\\| ==$\\|[ \t]*$\\|" page-delimiter))
  (setq-local paragraph-separate (concat " *-- ==$\\|[ \t]*$\\|" page-delimiter))
  (setq-local comment-padding " ")
  (setq-local font-lock-defaults '(futhark-highlight-font-lock))
  (futhark-indent-setup))

(provide 'futhark-mode)

;;; futhark-mode.el ends here
