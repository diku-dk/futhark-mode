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

(defun futhark-eldoc-function ()
  (let* ((info (futhark-lookup-info-at-point))
         (name (cdr (assoc 'name info)))
         (type (cdr (assoc 'type info))))
    (when (and name type)
      (format "%s: %s" name type))))

(provide 'futhark-eldoc)

;;; futhark-eldoc.el ends here
