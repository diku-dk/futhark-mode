;;; futhark-test.el --- run "futhark test" from futhark-mode  -*- lexical-binding: t; -*-

;; Copyright (C) DIKU 2013-2019, University of Copenhagen

;; This file is not part of GNU Emacs.

;;; License:
;; ICS <https://github.com/diku-dk/futhark-mode/blob/master/LICENSE>

;;; Commentary:
;; See futhark-mode.el.

;;; Code:

(require 'ansi-color)
(require 'compile)

(defcustom futhark-test-program "futhark"
  "The \"futhark\" executable used by `futhark-test'."
  :type 'string
  :group 'futhark)

(defcustom futhark-test-default-options "--backend=c"
  "Options offered by default when `futhark-test' asks for some."
  :type 'string
  :group 'futhark)

(defvar futhark-test-options-history nil
  "Minibuffer history of options given to `futhark-test'.")

(defvar futhark-test-buffer-name "*futhark-test*"
  "Name of the buffer in which `futhark-test' shows its output.")

(defun futhark-test-read-options ()
  "Ask for the options to give to \"futhark test\".
`futhark-test-default-options' is put in the minibuffer for editing."
  (read-string "Options: " futhark-test-default-options
               'futhark-test-options-history))

(defun futhark-test-command (options file)
  "Build the shell command testing FILE with OPTIONS.
OPTIONS is a line of shell words and is used as written."
  (mapconcat
   #'identity
   (delete ""
           (list (shell-quote-argument futhark-test-program)
                 "test"
                 "--no-terminal"
                 options
                 (shell-quote-argument file)))
   " "))

(defconst futhark-test-error-regexp
  (list "^\\(?:Error\\|\\(Warning\\)\\) at \\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)"
        2 3 4 '(1))
  "How \"futhark test\" reports the position of a problem.
An entry for `compilation-error-regexp-alist', so that
\\[next-error] walks the errors in the test output.")

(defun futhark-test-colorize ()
  "Turn the ANSI escapes in freshly inserted output into faces.
\"futhark test\" colours its output whenever it is talking to a
terminal, and Emacs gives it one."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(define-compilation-mode futhark-test-mode "Futhark-Test"
  "Mode for the output of \"futhark test\"."
  (setq-local compilation-error-regexp-alist
              (cons futhark-test-error-regexp
                    (default-value 'compilation-error-regexp-alist)))
  (add-hook 'compilation-filter-hook #'futhark-test-colorize nil t))

;;;###autoload
(defun futhark-test (options)
  "Run \"futhark test\" on the file of the current buffer with OPTIONS.

OPTIONS is a line of shell words, such as \"--backend=opencl -t\".
Interactively, it is read from the minibuffer, prefilled with
`futhark-test-default-options' so that it can be edited.  Output
appears as it is produced in the buffer named by
`futhark-test-buffer-name'.

Since \"futhark test\" reads the program from disk, this offers to
save modified buffers first, as \\[compile] does."
  (interactive (list (futhark-test-read-options)))
  (let ((file (or buffer-file-name
                  (user-error "Buffer %s is not visiting a file"
                              (buffer-name)))))
    (save-some-buffers (not compilation-ask-about-save)
                       compilation-save-buffers-predicate)
    (compilation-start (futhark-test-command options file)
                       #'futhark-test-mode
                       (lambda (_name-of-mode) futhark-test-buffer-name))))

(provide 'futhark-test)

;;; futhark-test.el ends here
