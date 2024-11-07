;;; futhark-fmt.el --- Format SML source code using the "futhark fmt" program -*- lexical-binding: t -*-

;; Copyright (C) DIKU 2013-2019, University of Copenhagen

;; This file is not part of GNU Emacs.

;;; License:
;; ICS <https://github.com/diku-dk/futhark-mode/blob/master/LICENSE>

;;; Commentary:
;; See futhark-mode.el.

;;; Code:

(require 'reformatter)

(defgroup futhark-fmt nil
  "Integration with the \"futhark fmt\" formatting program."
  :prefix "futhark-fmt-"
  :group 'sml)

(defcustom futhark-fmt-extra-args '()
  "Extra arguments to give to \"futhark fmt\"."
  :group 'futhark-fmt
  :type 'sexp
  :safe #'listp)

(defvar futhark-fmt-mode-map (make-sparse-keymap)
  "Local keymap used for `futhark-fmt-format-on-save-mode`.")

;;;###autoload (autoload 'futhark-fmt-buffer "futhark-fmt" nil t)
;;;###autoload (autoload 'futhark-fmt-region "futhark-fmt" nil t)
;;;###autoload (autoload 'futhark-fmt-on-save-mode "futhark-fmt" nil t)
(reformatter-define futhark-fmt
  :program "futhark"
  :args (cons "fmt" futhark-fmt-extra-args)
  :group 'futhark-fmt
  :lighter " Fmt"
  :keymap futhark-fmt-mode-map)

(defalias 'futhark-fmt 'futhark-fmt-format-buffer)

(provide 'futhark-fmt)

;;; futhark-fmt.el ends here
