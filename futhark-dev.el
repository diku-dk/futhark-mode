;;; futhark-dev.el --- developer tools for futhark-mode  -*- lexical-binding: t; -*-

;; Copyright (C) DIKU 2013-2019, University of Copenhagen

;; This file is not part of GNU Emacs.

;;; License:
;; ICS <https://github.com/diku-dk/futhark-mode/blob/master/LICENSE>

;;; Commentary:
;; This file is not included by futhark-mode.el, but rather contains useful
;; functions for hacking on the mode.

;;; Code:

(require 'cl-lib)

(require 'futhark-mode)

(defun futhark-dev-reload ()
  "FOR DEVELOPMENT: Unload and re-require all of futhark-mode.
Also reapply futhark-mode to all futhark-mode buffers."
  (interactive)

  (let ((futhark-mode-buffers
         (cl-remove-if-not (lambda (buf)
                             (with-current-buffer buf
                               (eq major-mode 'futhark-mode)))
                           (buffer-list))))

    (ignore-errors (unload-feature 'futhark-mode t))
    (ignore-errors (unload-feature 'futhark-highlight t))
    (ignore-errors (unload-feature 'futhark-indent t))
    (ignore-errors (unload-feature 'futhark-comint t))
    (ignore-errors (unload-feature 'futhark-flycheck t))

    (require 'futhark-mode)

    (mapc (lambda (buf)
            (with-current-buffer buf
              (futhark-mode)))
          futhark-mode-buffers)))

(provide 'futhark-dev)

;;; futhark-dev.el ends here
