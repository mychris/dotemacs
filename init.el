;;; init.el --- Emacs configuration file              -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs configuration file.
;; Does some basic setup (primarily for use-package)
;; and then loads the custom settings from the settings.org mode file.
;; Do not edit settings.el manually.
;; The file is generated from the settings.org file, if required.

;;; Code:

;;;; Customization information

(when (version< emacs-version "27")
  (error "Emacs version < 27 no supported"))

(when (not (member system-type '(gnu/linux)))
  (error "Unsupported operating system %s" system-type))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; This is a weird one, see
;; https://emacshorrors.com/posts/advertising-your-freedom.html
(fset 'display-startup-echo-area-message 'ignore)

;;;; Custom load-path

(add-to-list 'load-path (file-name-as-directory (expand-file-name "elisp" user-emacs-directory)))

;;;; Make sure some environment variables are set

(unless (getenv "XDG_CACHE_HOME")
  (setenv "XDG_CACHE_HOME" (expand-file-name "~/.cache")))

(unless (getenv "EMACS_CACHE_DIR")
  (setenv "EMACS_CACHE_DIR" (expand-file-name "emacs" (getenv "XDG_CACHE_HOME"))))

(defvar user-emacs-cache-directory
  (getenv "EMACS_CACHE_DIR")
  "Directory for user specific Emacs cache files.")

(unless (file-exists-p user-emacs-cache-directory)
  (make-directory user-emacs-cache-directory))

;;;; Setup native compilation

(when (version<= "28" emacs-version)
  (eval-when-compile
    (require 'comp))
  ;; Debug level for native compilation, a number between 0 and 3.
  (setq native-comp-debug 0)
  ;; Optimization level for native compilation, a number between -1 and 3.
  ;; -1 functions are kept in bytecode form and no native compilation is performed.
  ;;  0 native compilation is performed with no optimizations.
  ;;  1 light optimizations.
  ;;  2 max optimization level fully adherent to the language semantic.
  ;;  3 max optimization level, to be used only when necessary.
  ;;    Warning: with 3, the compiler is free to perform dangerous optimizations.
  (setq native-comp-speed 2)
  ;; Non-nil to prevent native-compiling of Emacs Lisp code.
  (setq no-native-compile nil)
  ;; Non-nil means to natively compile packages as part of their installation.
  (setq package-native-compile t)
  ;; Whether to report warnings and errors from asynchronous native compilation.
  (setq native-comp-async-report-warnings-errors 'silent))

;;;; Bootstrap use-package

;; this should already be loaded, but I got an error in emacs-27.1 for some reason.
;; TODO: investigate
(unless (boundp 'package-archives)
  (require 'package))
(require 'nsm)
(require 'gnutls)
(setq network-security-level 'high)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-quickstart-file (expand-file-name "package-quickstart.el" user-emacs-cache-directory))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-quickstart-refresh))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(unless (package-installed-p 'delight)
  (package-refresh-contents)
  (package-install 'delight))

(eval-when-compile
  (require 'cl-lib))
(require 'diminish)
(require 'delight)
(require 'bind-key)
(require 'my-utils)

(eval-and-compile
  (require 'use-package)
  ;; Whether to report about loading and configuration details.
  (setq use-package-verbose nil);(not (bound-and-true-p byte-compile-current-file)))
  ;; If non-nil, compute statistics concerned use-package declarations.
  (setq use-package-compute-statistics nil);(not (bound-and-true-p byte-compile-current-file)))
  ;; If non-nil, assume ':demand t' unless ':defer' is used.
  (setq use-package-always-demand nil)
  ;; If non-nil, assume ':defer t' unless ':demand' is used.
  (setq use-package-always-defer t)
  ;; Treat every package as though it had specified using ':pin SYM'
  (setq use-package-always-pin nil)
  ;; Treat every package as though it had specified using ':ensure SEXP'
  (setq use-package-always-ensure nil)
  ;; If non-nil, make the expanded code as minimal as possible.
  (setq use-package-expand-minimally nil)
  ;; If non-nil, cause imenu to see 'use-package' declarations.
  (setq use-package-enable-imenu-support t)
  ;; Text append to the name of hooks mentioned by :hook.
  (setq use-package-hook-name-suffix nil))

;;;; Load settings

(defun my/file-time-less-p (a b)
  "Return non-nil if the file modification time for `A' is less than `B'."
  (time-less-p
   (file-attribute-modification-time (file-attributes a))
   (file-attribute-modification-time (file-attributes b))))

(defun my/org-babel-load-file (file-org &optional do-byte-compile)
  "Load the given `FILE-ORG' using `org-bable-load-file'.
Also byte compiles the file and use the cached .elc, if `DO-BYTE-COMPILE'
evaluates to a non-nil value."
  (let* ((file-el (concat (file-name-sans-extension file-org) ".el")))
    (if (not (file-exists-p file-org))
        (error "Org file '%s' missing" file-org)
      (when (not (and (file-exists-p file-el)
               (my/file-time-less-p file-org file-el)))
        (progn
          (require 'org)
          (org-babel-tangle-file file-org file-el "emacs-lisp")
          (when do-byte-compile
            (byte-compile-file file-el))))
      ;; found that one somewhere, shaves off another 200ms during startup.
      ;; no idea if this has any negative side effects, nothing emerged yet.
      (let ((file-name-handler-alist nil))
        (load (file-name-sans-extension file-el))))))

;; For now, do not byte compile the settings files. For some reason, this
;; messes up some of the use-package declaration.
(let ((main-settings (expand-file-name "settings.org" user-emacs-directory))
      (local-settings (expand-file-name "settings-local.org" user-emacs-directory)))
  (my/org-babel-load-file main-settings)
  (when (file-exists-p local-settings)
    (my/org-babel-load-file local-settings)))

;;; init.el ends here
