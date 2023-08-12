;; abbrev_defs.eld --- Abbrev definitions. -*- lexical-binding: t; coding: utf-8; -*-


;;; c-mode
(define-abbrev-table 'c-mode-global-abbrev-table
  '(
    ("inclued" "include" nil :count 0)
    ("uinon" "union" nil :count 0)
    )
  "Abbrev table for `c-mode' expanding anywhere."
  :enable-function #'+abbrev-enable-globally)
(define-abbrev-table 'c-mode-code-abbrev-table
  '(
    ("sizet" "size_t" nil :count 0)
    ("ssizet" "ssize_t" nil :count 0)
    ("offt" "off_t" nil :count 0)
    ("off64t" "off64_t" nil :count 0)
    ("pidt" "pid_t" nil :count 0)
    ("pthreadt" "pthread_t" nil :count 0)
    ("timet" "time_t" nil :count 0)
    ("int8t" "int8_t" nil :count 0)
    ("int16t" "int16_t" nil :count 0)
    ("int32t" "int32_t" nil :count 0)
    ("int64t" "int64_t" nil :count 0)
    ("intmaxt" "intmax_t" nil :count 0)
    ("intptrt" "intptr_t" nil :count 0)
    ("uint8t" "uint8_t" nil :count 0)
    ("uint16t" "uint16_t" nil :count 0)
    ("uint32t" "uint32_t" nil :count 0)
    ("uint64t" "uint64_t" nil :count 0)
    ("uintmaxt" "uintmax_t" nil :count 0)
    ("uintptrt" "uintptr_t" nil :count 0)
    ("ptrdifft" "ptrdiff_t" nil :count 0)
    ("ii" "int i" nil :count 0)
    )
  "Abbrev table for `c-mode' expanding only for code."
  :enable-function #'+abbrev-enable-in-code)
(define-abbrev-table 'c-mode-abbrev-table
  '()
  "Abbrev table for `c-mode'.
Parents: `c-mode-code-abbrev-table' `c-mode-global-abbrev-table'."
  :parents (list c-mode-code-abbrev-table c-mode-global-abbrev-table))


;;; c++-mode
(define-abbrev-table 'c++-mode-global-abbrev-table
  '()
  "Abbrev table for `c++-mode' expanding anywhere."
  :enable-function #'+abbrev-enable-globally
  :parents (list c-mode-global-abbrev-table))
(define-abbrev-table 'c++-mode-code-abbrev-table
  '(
    ("ccast" "const_cast" nil :count 0)
    ("scast" "static_cast" nil :count 0)
    ("rcast" "reinterpret_cast" nil :count 0)
    ("dcast" "dynamic_cast" nil :count 0)
    )
  "Abbrev table for `c++-mode' expanding only for code."
  :enable-function #'+abbrev-enable-in-code
  :parents (list c-mode-code-abbrev-table))
(define-abbrev-table 'c++-mode-abbrev-table
  '()
  "Abbrev table for `c-mode'.
Parents: `c++-mode-code-abbrev-table' `c++-mode-global-abbrev-table'."
  :parents (list c++-mode-code-abbrev-table c++-mode-global-abbrev-table))


;;; emacs-list-mode
(define-abbrev-table 'emacs-lisp-mode-global-abbrev-table
  '()
  "Abbrev table for `emacs-lisp-mode' expanding anywhere."
  :enable-function #'+abbrev-enable-globally)
(define-abbrev-table 'emacs-lisp-mode-code-abbrev-table
  '(
    ("defun" "defun" +abbrev-yas-hook :count 0)
    ("lambda" "lambda" +abbrev-yas-hook :count 0)
    ("setq" "setq" +abbrev-yas-hook :count 0)
    ("let" "let" +abbrev-yas-hook :count 0)
    ("autoload" "autoload" +abbrev-yas-hook :count 0)
    ("usep" "usep" +abbrev-yas-hook :count 0)
    )
  "Abbrev table for `emacs-lisp-mode' only for code."
  :enable-function #'+abbrev-enable-in-code)
(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '()
  "Abbrev table for `emacs-lisp-mode'.
Parents: `emacs-lisp-mode-code-abbrev-table' `emacs-lisp-mode-global-abbrev-table'."
  :parents (list emacs-lisp-mode-code-abbrev-table emacs-lisp-mode-global-abbrev-table))


;;; global
(define-abbrev-table 'global-abbrev-table
  '(
    ("openjdk" "OpenJDK" nil :count 0)
    ("gcc" "GCC" nil :count 0)
    ("emacs" "Emacs" nil :count 0)
    ))

