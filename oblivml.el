;; The current approach is to define ELisp style regular expressions that match
;; the regular expressions in the lexer (src/lexer.mll). Then, use the ELisp regular
;; expressions to match the character classes and match them up with faces.

;; Current issue: the lexer matches `in` before it matches `int` ... that shouldn't work, should it?

(defvar oblivml-mode-syntax-table nil "Syntax table for `oblivml-mode'.")

(setq oblivml-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; Wolfram Language style comment “(* … *)”
        (modify-syntax-entry ?\( ". 1" synTable)
        (modify-syntax-entry ?\) ". 4" synTable)
        (modify-syntax-entry ?* ". 23" synTable)
        synTable))

(setq oblivml-unit "()")
(setq oblivml-bool "true\\|false")
(setq oblivml-int "-?[0-9]+")
(setq oblivml-label "public\\|secret")
(setq oblivml-rbottom "_|_")
(setq oblivml-rvar "`[a-z A-Z][0 - 9]")

(setq oblivml-highlights
      `((,oblivml-unit . font-lock-constant-face)
        (,oblivml-bool . font-lock-constant-face)
        (,oblivml-int . font-lock-constant-face)
        (,oblivml-label . font-lock-type-face)
        (,oblivml-rbottom . font-lock-type-face)
        (,oblivml-rvar . font-lock-type-face)))

(define-derived-mode
  oblivml-mode
  fundamental-mode
  "OblivML"
  "Major mode for editing OblivML programs."
  (setq font-lock-defaults '(oblivml-highlights)))
