;;; futhark-eldoc.el --- ElDoc for futhark-mode  -*- lexical-binding: t; -*-

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

(defun futhark-get-info-field (field string)
  (when (string-match (format "^%s: \\(.*\\)$" (regexp-quote field)) string)
    (match-string 1 string)))

(defun futhark-lookup-info-at-point (&optional pos)
  (unless (buffer-file-name)
    (error "Buffer is not associated with a file"))
  (let* ((pos (or pos (point)))
         (line (line-number-at-pos pos))
         (col (save-excursion (goto-char pos) (1+ (current-column))))
         (command (format "futhark query %s %d %d" (buffer-file-name) line col))
         (output (shell-command-to-string command)))
    (list (cons 'name (futhark-get-info-field "Name" output))
          (cons 'position (futhark-get-info-field "Position" output))
          (cons 'definition (futhark-get-info-field "Definition" output))
          (cons 'type (futhark-get-info-field "Type" output)))))

(defun futhark-eldoc-function ()
  (let* ((info (futhark-lookup-info-at-point))
         (name (cdr (assoc 'name info)))
         (type (cdr (assoc 'type info))))
    (when (and name type)
      (format "%s: %s" name type))))

(provide 'futhark-eldoc)

;;; futhark-eldoc.el ends here
