;;; futhark-comint.el --- interpreter interaction for futhark-mode  -*- lexical-binding: t; -*-

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

(require 'comint)

(defcustom futhark-comint-interpreter-name "futhark"
  "Futhark interpreter to run, not including options."
  :type 'string :group 'futhark)

(defcustom futhark-comint-interpreter-args '("repl")
  "Command line passed to `futhark-comint-interpreter-name', if any."
  :type '(repeat string) :group 'futhark)

(defvar futhark-comint-prompt-regexp "^\\(?:\\[[0-9]+\\]\\)"
  "Prompt for `run-futhark'.")

(defun run-futhark ()
  "Run an inferior instance of `futharki' inside Emacs."
  (interactive)
  (pop-to-buffer
   (apply 'make-comint "futhark" futhark-comint-interpreter-name nil futhark-comint-interpreter-args))
  (futhark-comint-mode))

(defvar futhark-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    map)
  "Keymap for `futhark-comint-mode'.")

(define-derived-mode futhark-comint-mode comint-mode "futhark"
  "Major mode for `run-futhark'.

\\<futhark-comint-mode-map>"
  nil "futhark"
  (setq comint-prompt-regexp futhark-comint-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  (set (make-local-variable 'paragraph-start) futhark-comint-prompt-regexp))

(defun futhark-comint-load-file (file)
  "Load FILE into the futharki process.
FILE is the file visited by the current buffer.

Automatically starts an inferior futharki process with `run-futhark`
if a running futharki instance cannot be found."
  (interactive
   (list (or buffer-file-name
             (read-file-name "File to load: " nil nil t))))
  (comint-check-source file)
  (let ((b (get-buffer "*futhark*"))
        (p (get-process "futhark")))
    (if (and b p)
        (progn
         (with-current-buffer b
           (apply comint-input-sender (list p (concat ":load " file))))
         (pop-to-buffer b))
      (run-futhark)
      (futhark-comint-load-file file))))

(provide 'futhark-comint)

;;; futhark-comint.el ends here
