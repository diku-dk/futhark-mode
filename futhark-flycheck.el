;;; futhark-flycheck.el --- flycheck definition for futhark-mode  -*- lexical-binding: t; -*-

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
;; See futhark-mode.el.

;;; Code:

(require 'flycheck nil t) ;; no error if not found (flycheck is optional)

(when (featurep 'flycheck)
  (flycheck-define-checker futhark
    "A Futhark syntax and type checker.
See URL `https://github.com/diku-dk/futhark'."
    :command ("futhark" "check" source-inplace)
    :modes 'futhark-mode
    :error-parser flycheck-parse-with-patterns-without-color
    :error-patterns
    ((error line-start "Error at " (file-name) ":" line ":" column "-"
            (optional (one-or-more not-newline) ":") (message (one-or-more anything))
            "If you find")
     (error line-start "Unexpected end of file")
     (error (message "lexical error") " at line " line ", column " column)
     (warning line-start "Warning at " (file-name) ":"
              line ":" column "-" (one-or-more digit) ":" (one-or-more digit) ":" ?\n
              (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
              line-end)))
  (add-to-list 'flycheck-checkers 'futhark))

(provide 'futhark-flycheck)

;;; futhark-flycheck.el ends here
