;;; futhark-const.el --- constants for futhark-mode  -*- lexical-binding: t; -*-

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
;; See futhark-mode.el.

;;; Code:

(defconst futhark-const-ws "[[:space:]\n]*")
(defconst futhark-const-ws1 "[[:space:]\n]+")

;; FIXME: Backslash should also be a keyword (for anonymous functions), but
;; Emacs Lisp is stupid.
(defconst futhark-const-keywords
  '("if" "then" "else" "let" "loop" "in" "with" "type"
    "val" "entry" "for" "while" "do" "case" "match"
    "unsafe" "include" "import" "module" "open" "local" "assert")
  "All Futhark keywords.")

(defconst futhark-const-builtin-functions
  '("zip" "unzip" "map" "reduce"
    "reduce_comm" "scan" "filter" "partition" "scatter" "stream_map"
    "stream_map_per" "stream_red" "stream_map_per" "stream_seq"
    "reduce_by_index")
  "All Futhark builtin SOACs, functions, and non-symbolic operators.")

(defconst futhark-const-numeric-types
  '("i8" "i16" "i32" "i64"
    "u8" "u16" "u32" "u64"
    "f32" "f64")
  "A list of Futhark numeric types.")

(defconst futhark-const-builtin-types
  (cons "bool" futhark-const-numeric-types)
  "A list of Futhark types.")

(defconst futhark-const-booleans
  '("true" "false")
  "All Futhark booleans.")

(defconst futhark-const-number
  (concat "-?"
          "\\<\\(?:"
          (concat "\\(?:"
                  "\\(?:0[xX]\\)"
                  "[0-9a-fA-F]+"
                  "\\(?:\\.[0-9a-fA-F]+\\)?"
                  "\\(?:[pP][+-]?[0-9]+\\)?"
                  "\\|"
                  "[0-9]+"
                  "\\(?:\\.[0-9]+\\)?"
                  "\\(?:e-?[0-9]+\\)?"
                  "\\)"
                  )
          "\\(?:i8\\|i16\\|i32\\|i64\\|u8\\|u16\\|u32\\|u64\\|f32\\|f64\\)?"
          "\\)\\>")
  "All numeric constants, including hex float literals.")

(defconst futhark-const-character
  (concat "'[^']?'"))

(defconst futhark-const-var
  (concat "\\(?:" "[_'[:alnum:]]+" "\\)")
  "A regex describing a Futhark variable.")

(defconst futhark-const-constructor
  (concat "\\(?:" "#[_'[:alnum:]]+" "\\)")
  "A regex describing a Futhark constructor.")

(defconst futhark-const-operator
  (concat "\\(?:"
          (concat "["
                  "-+*/%!<>=&|@"
                  "]" "+")
          "\\|"
          "`[^`]*`"
          "\\)"))

(defconst futhark-const-non-tuple-type
  (concat "\\(?:"
          "\\*" "?"
          "\\(?:"
          "\\["
          "\\(?:"
          ""
          "\\|"
          futhark-const-var
          "\\)"
          "\\]"
          "\\)" "*"
          futhark-const-var
          "\\)"
          )
  "A regex describing a Futhark type which is not a tuple.")

;; This does not work with nested tuple types.
(defconst futhark-const-tuple-type
  (concat "\\(?:"
          "("
          "\\(?:" futhark-const-ws futhark-const-non-tuple-type futhark-const-ws "," "\\)" "*"
          futhark-const-ws futhark-const-non-tuple-type futhark-const-ws
          ")"
          "\\)"
          )
  "A regex describing a Futhark type which is a tuple.")

(defconst futhark-const-type
  (concat "\\(?:"
          futhark-const-non-tuple-type
          "\\|"
          futhark-const-tuple-type
          "\\)"
          )
  "A regex describing a Futhark type.")

(provide 'futhark-const)

;;; futhark-const.el ends here
