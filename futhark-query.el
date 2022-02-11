;;; futhark-eldoc.el --- interface to 'futhark query'  -*- lexical-binding: t; -*-

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
         (args (list "query"
                     (buffer-file-name)
                     (number-to-string line)
                     (number-to-string col)))
         (output (with-output-to-string
                   (with-current-buffer
                       standard-output
                     (apply 'call-process "futhark" nil t nil args)))))
    (let ((name (futhark-get-info-field "Name" output))
          (position (futhark-get-info-field "Position" output))
          (definition (futhark-get-info-field "Definition" output))
          (type (futhark-get-info-field "Type" output)))
     (append (when name (list (cons 'name name)))
             (when position (list (cons 'position position)))
             (when definition (list (cons 'definition definition)))
             (when type (list (cons 'type type)))))))

;;; Go to definition;
;;
;; In a perfect world, this would be built with the xref system, but
;; it looks very complicated, so for now we hack it.

(defcustom futhark-definition-ring-length 16
  "Length of `futhark-definition-ring`."
  :type 'integer
  :group 'futhark)

(defvar futhark-definition-ring (make-ring futhark-definition-ring-length)
  "Ring of markers to implement definition stack.")

(defun futhark-push-marker-stack (&optional m)
  "Add point M (defaults to `point-marker') to the definition stack."
  (ring-insert futhark-definition-ring (or m (point-marker))))

(defun futhark-go-to-definition ()
  "Go to the definition of the name under point.  Return with
  `futhark-return-from-definition`."
  (interactive)
  (let ((info (futhark-lookup-info-at-point)))
    (unless info
      (error "No name at point"))
    (let ((def (cdr (assoc 'definition info))))
      (unless def
        (error "Cannot find definition of %s" (cdr (assoc 'name info))))
      ;; E.g. /foo/bar.fut:195:1-212:32
      (string-match "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+:\\)?[0-9]+" def)
      (futhark-push-marker-stack)
      (let ((file (match-string 1 def))
            (line (string-to-number (match-string 2 def)))
            (col (1- (string-to-number (match-string 3 def)))))
        (if (string-match "^/futlib/" file)
            (message "%s is a built-in" (cdr (assoc 'name info)))
          (find-file file)
          (goto-char (point-min))
          (forward-line (1- line))
          (move-beginning-of-line nil)
          (forward-char col))))))

(defun futhark-return-from-definition ()
  "Return to the editing position before the previous invocation
of `futhark-go-to-definition`."
  (interactive)
  (let ((ring futhark-definition-ring))
    (when (ring-empty-p ring)
      (user-error "No definition to return from"))
    (let ((marker (ring-remove ring 0)))
      (switch-to-buffer (or (marker-buffer marker)
                            (user-error "The previous buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil nil))))

(provide 'futhark-query)

;;; futhark-query.el ends here
