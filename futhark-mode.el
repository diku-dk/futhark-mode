;;; futhark-mode.el --- major mode for editing Futhark source files  -*- lexical-binding: t; -*-

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
;; Futhark is a small programming language designed to be compiled to efficient
;; GPU code.  This Emacs mode provides syntax highlighting, conservative
;; automatic indentation, interpreter interaction, and an optional flycheck
;; definition.
;;
;; Files with the ".fut" extension are automatically handled by this mode.
;;
;; For extensions: Define local keybindings in `futhark-mode-map'.  Add startup
;; functions to `futhark-mode-hook'.

;;; Code:

(require 'futhark-const)
(require 'futhark-highlight)
(require 'futhark-indent)
(require 'futhark-comint)
(require 'futhark-flycheck)

(defgroup futhark ()
  "Editing Futhark source files."
  :group 'languages)

(defvar futhark-mode-hook nil
  "Hook for `futhark-mode'.  Is run whenever the mode is entered.")

(defvar futhark-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-c\C-l" 'futhark-comint-load-file)
    map)
  "Keymap for `futhark-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fut\\'" . futhark-mode))

;;;###autoload
(define-derived-mode futhark-mode prog-mode "Futhark"
  "Major mode for editing Futhark source files."
  :syntax-table futhark-highlight-syntax-table
  (setq-local font-lock-defaults '(futhark-highlight-font-lock))
  (setq-local indent-line-function 'futhark-indent-line)
  (setq-local indent-region-function nil)
  (setq-local comment-start "--")
  (setq-local comment-start-skip "--[ \t]*")
  (setq-local paragraph-start (concat " *-- |\\| ==$\\|[ \t]*$\\|" page-delimiter))
  (setq-local paragraph-separate (concat " *-- ==$\\|[ \t]*$\\|" page-delimiter))
  (setq-local comment-padding " "))

(provide 'futhark-mode)

;;; futhark-mode.el ends here
