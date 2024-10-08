;;; futhark-highlight.el --- highlighting for futhark-mode  -*- lexical-binding: t; -*-

;; Copyright (C) DIKU 2013-2019, University of Copenhagen

;; This file is not part of GNU Emacs.

;;; License:
;; ICS <https://github.com/diku-dk/futhark-mode/blob/master/LICENSE>

;;; Commentary:
;; See futhark-mode.el.

;;; Code:

(defun futhark-highlight-syms-re (syms)
  "Create a regular expression matching any of the symbols in SYMS.
This is useful because the normal word boundaries are not what we
want (for example, we consider underscore to be part of a symbol
name)."
  (concat "\\_<" (regexp-opt syms t) "\\_>"))

(defconst futhark-highlight-ws "[[:space:]\n]*")
(defconst futhark-highlight-ws1 "[[:space:]\n]+")

(defconst futhark-highlight-keywords
  '("if" "then" "else" "let" "loop" "in" "with" "type"
    "val" "entry" "for" "while" "do" "case" "match"
    "include" "import" "module" "open" "local" "assert" "def")
  "All Futhark keywords.")

(defconst futhark-highlight-numeric-types
  '("i8" "i16" "i32" "i64"
    "u8" "u16" "u32" "u64"
    "f16" "f32" "f64")
  "A list of Futhark numeric types.")

(defconst futhark-highlight-builtin-types
  (cons "bool" futhark-highlight-numeric-types)
  "A list of Futhark types.")

(defconst futhark-highlight-booleans
  '("true" "false")
  "All Futhark booleans.")

(defconst futhark-highlight-number
  (concat "-?"
          "\\<\\(?:"
          (concat "\\(?:"
                  "\\(?:0[xX]\\)"
                  "[0-9a-fA-F_]+"
                  "\\(?:\\.[0-9a-fA-F_]+\\)?"
                  "\\(?:[pP][+-]?[0-9_]+\\)?"
                  "\\|"
                  "\\(?:0[bB]\\)"
                  "[0-1_]+"
                  "\\|"
                  "[0-9_]+"
                  "\\(?:\\.[0-9_]+\\)?"
                  "\\(?:e-?[0-9_]+\\)?"
                  "\\)"
                  )
          "\\(?:i8\\|i16\\|i32\\|i64\\|u8\\|u16\\|u32\\|u64\\|f16\\|f32\\|f64\\)?"
          "\\)\\>")
  "All numeric constants, including hex float literals.")

(defconst futhark-highlight-character
  (concat "'\\\\?[^']?'"))

(defconst futhark-highlight-var
  (concat "\\(?:" "[_[:alpha:]][_'[:alnum:]]*" "\\)")
  "A regex describing a Futhark variable.")

(defconst futhark-highlight-hole
  (concat "\\(?:" "\\S." "\\?\\?\\?" "\\S." "\\)")
  "A regex describing a Futhark typed holed.")

(defconst futhark-highlight-constructor
  (concat "\\(?:" "#[_'[:alnum:]]+" "\\)")
  "A regex describing a Futhark constructor.")

(defconst futhark-highlight-operator
  (concat "\\(?:"
          (concat "["
                  "-+*/%!<>=&|@"
                  "]"
                  "["
                  "-+*/%!<>=&|@"
                  "]" "+") ; don't highlight '=', but do highlight '=='
          "\\|"
          (concat "["
                  "-+*/%!<>&|@"
                  "]")
          "\\|"
          "`[^`]*`"
          "\\)"))

(defconst futhark-highlight-non-tuple-type
  (concat "\\(?:"
          "\\*" "?"
          "\\(?:"
          "\\["
          "\\(?:"
          ""
          "\\|"
          futhark-highlight-var
          "\\(?:" "\\." futhark-highlight-var "\\)" "?"
          "\\)"
          "\\]"
          "\\)" "*"
          futhark-highlight-var
          "\\(?:" "\\." futhark-highlight-var "\\)" "?"
          "\\)"
          )
  "A regex describing a Futhark type which is not a tuple.")

;; This does not work with nested tuple types.
(defconst futhark-highlight-tuple-type
  (concat "\\(?:"
          "("
          "\\(?:" futhark-highlight-ws futhark-highlight-non-tuple-type futhark-highlight-ws "," "\\)" "*"
          futhark-highlight-ws futhark-highlight-non-tuple-type futhark-highlight-ws
          ")"
          "\\)"
          )
  "A regex describing a Futhark type which is a tuple.")

(defconst futhark-highlight-type
  (concat "\\(?:"
          futhark-highlight-non-tuple-type
          "\\|"
          futhark-highlight-tuple-type
          "\\)"
          )
  "A regex describing a Futhark type.")

(defconst futhark-highlight-attribute
  (concat "\\(?:" "#\\[[^]]+\\]" "\\)")
  "A regex describing a Futhark constructor.")

(defvar futhark-highlight-font-lock
  `(

    ;; Variable and tuple declarations.

      ;;; Value specs.
    (,(concat "val" futhark-highlight-ws1
              "\\(" futhark-highlight-var "\\)")
     . '(1 font-lock-function-name-face))

    ;;; Definitions.
    (,(concat "def" futhark-highlight-ws1
              "\\(" futhark-highlight-var "\\)")
     . '(1 font-lock-function-name-face))

      ;;; Lets.
      ;;;; Primitive values.
    (,(concat "let" futhark-highlight-ws1
              "\\(" futhark-highlight-var "\\)"
              futhark-highlight-ws "=")
     . '(1 font-lock-variable-name-face))
      ;;;; In-place updates.
    (,(concat "let" futhark-highlight-ws1
              "\\(" futhark-highlight-var "\\)"
              "\\[")
     . '(1 font-lock-variable-name-face))
      ;;;; Tuples.  XXX: It would be nice to highlight only the variable names
      ;;;; inside the parantheses, and not also the commas.
    (,(concat "let" futhark-highlight-ws1 "("
              "\\(" "[^)]+" "\\)")
     . '(1 font-lock-variable-name-face))
      ;;; Function parameters.
    (,(concat "\\(" futhark-highlight-var "\\)" ":")
     . '(1 font-lock-variable-name-face))

    ;; Constants.
      ;;; Characters
    (,(concat "\\(" futhark-highlight-character "\\)")
     . font-lock-constant-face)

      ;;; Constructors
    (,(concat "\\(" futhark-highlight-constructor "\\)")
     . font-lock-constant-face)

      ;;; Booleans.
    (,(futhark-highlight-syms-re futhark-highlight-booleans)
     . font-lock-constant-face)

      ;;; Numbers
    (,(concat "\\S_\\(" futhark-highlight-number "\\)")
     . (1 font-lock-constant-face))

      ;;; Holes
    (,(concat "\\(" futhark-highlight-hole "\\)")
     . font-lock-warning-face)

    ;; Keywords.
    ;; Placed after constants, so e.g. '#open' is highlighted
    ;; as a value and not as a keyword.
    (, (concat "\\(" (futhark-highlight-syms-re futhark-highlight-keywords)
               "\\|" "\\\\" ; lambda
               "\\)")
     . font-lock-keyword-face)

    ;; Attributes.
    (,(concat "\\(" futhark-highlight-attribute "\\)")
     . font-lock-preprocessor-face)

    ;; Types.
    ;;; Type aliases.  XXX: It would be nice to highlight also the right hand
    ;;; side.
    (,(concat "type[~^]?" futhark-highlight-ws1 "\\(" futhark-highlight-type "\\)")
     . '(1 font-lock-type-face))
      ;;; Builtin types.
    (,(futhark-highlight-syms-re futhark-highlight-builtin-types)
     . font-lock-type-face)

    ;; Modules
    (,(concat "module" futhark-highlight-ws1 "\\(" futhark-highlight-var "\\)")
     . '(1 font-lock-type-face))

    ;; Functions.
      ;;; Function definitions.
    (,(concat "\\(?:"
              "let"
              "\\|"
              "entry"
              "\\)"
              futhark-highlight-ws1
              "\\(" futhark-highlight-var "\\)"
              futhark-highlight-ws
              "\\(?:"
              "\\[\\(?:[^]]\\)+\\]"
              futhark-highlight-ws
              "\\)" "*"
              "\\(?:"
              "("
              "\\|"
              futhark-highlight-ws1
              "[^=]"
              "\\)"
              )
     . '(1 font-lock-function-name-face))
      ;;; Operators.
    (,futhark-highlight-operator
     . font-lock-function-name-face))
  "Highlighting expressions for Futhark.")

(provide 'futhark-highlight)

;;; futhark-highlight.el ends here
