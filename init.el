;;; init.el --- Emacs configuration file              -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs configuration file.
;; Does some basic setup (primarily for use-package)
;; and then loads the custom settings from the settings.org file.
;; Do not edit settings.el manually.
;; The file is generated from the settings.org file, if required.

;;; Code:

;;;; Check supported emacs version and system

(when (version< emacs-version "27.1")
  ;; Internal features like `early-init.el' or `package-quickstart-refresh'
  ;; are used.
  ;; In addition, some packages like `vertico' are no longer supported in 26.3
  (error "Emacs version < 27.1 no supported"))

(when (not (member system-type '(gnu/linux windows-nt)))
  (error "Unsupported operating system %s" system-type))

;;;; Customization information

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;; Custom load-path

(add-to-list 'load-path (file-name-as-directory
			 (expand-file-name "elisp" user-emacs-directory)))

;;;; Setup the cache directory

(defconst user-emacs-cache-directory
  (let ((emacs-cache-dir-env (getenv "EMACS_CACHE_DIR"))
	(xdg-cache-home-env (getenv "XDG_CACHE_HOME")))
    (cond
     (emacs-cache-dir-env
      (file-name-as-directory emacs-cache-dir-env))
     (xdg-cache-home-env
      (file-name-as-directory (expand-file-name "emacs" xdg-cache-home-env)))
     ((eq system-type 'windows-nt)
      (file-name-as-directory (expand-file-name "emacs" (getenv "LOCALAPPDATA"))))
     (t
      (error "Failed to find Emacs cache directory"))))
  "Directory for user specific Emacs cache files.")

(setenv "EMACS_CACHE_DIR" user-emacs-cache-directory)

(unless (file-directory-p user-emacs-cache-directory)
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
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
	("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
	("nongnu-elpa" . "https://elpa.nongnu.org/nongnu/")
	("nongnu-elpa-devel" . "https://elpa.nongnu.org/nongnu-devel/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("gnu-elpa" . 10)
	("gnu-elpa-devel" . 9)
	("nongnu-elpa" . 8)
	("nongnu-elpa-devel" . 7)
	("melpa-stable" . 6)
	("melpa" . 5)))
(setq package-quickstart-file (expand-file-name "package-quickstart.el" user-emacs-cache-directory))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-quickstart-refresh))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'general)
  (package-refresh-contents)
  (package-install 'general))

(unless (package-installed-p 'delight)
  (package-refresh-contents)
  (package-install 'delight))

(eval-when-compile
  (require 'cl-lib))
(require 'delight)
(require 'bind-key)
(require 'my-utils)

(eval-and-compile
  (require 'use-package)
  ;; Whether to report about loading and configuration details.
  ;;(setq use-package-verbose (not (bound-and-true-p byte-compile-current-file)))
  (setq use-package-verbose nil)
  ;; If non-nil, compute statistics concerned use-package declarations.
  ;;(setq use-package-compute-statistics (not (bound-and-true-p byte-compile-current-file)))
  (setq use-package-compute-statistics nil)
  ;; Minimal load timet that will be reported
  (setq use-package-minimum-reported-time 0.001)
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
  (setq use-package-hook-name-suffix nil)
  ;; Quit the use-package-report window and kill the buffer as well.
  (define-key use-package-statistics-mode-map (kbd "q") #'kill-buffer-and-window))

;;;; Load settings

(defun +file-time-less-p (a b)
  "Return non-nil if the file modification time for A is less than B."
  (time-less-p
   (file-attribute-modification-time (file-attributes a))
   (file-attribute-modification-time (file-attributes b))))

(defun +org-babel-load-file (file-org-path &optional do-byte-compile)
  "Load the given FILE-ORG-PATH using `org-bable-load-file'.
The file is also byte-compiled, if DO-BYTE-COMPILE evaluates to a non-nil
value."
  (let* ((file-el-name (concat (file-name-base file-org-path) ".el"))
	 (file-el-path (expand-file-name file-el-name user-emacs-cache-directory))
	 (file-elc-path (byte-compile-dest-file file-el-path)))
    (if (not (file-exists-p file-org-path))
	(error "init.el: Org file '%s' not found" file-org-path)
      (when (not (and (file-exists-p file-el-path)
		      (+file-time-less-p file-org-path file-el-path)))
	(when (file-exists-p file-elc-path)
	  (delete-file file-elc-path))
	(require 'org)
	(org-babel-tangle-file file-org-path file-el-path "emacs-lisp")
	(when do-byte-compile
	  (unless (byte-compile-file file-el-path)
	    (error "init.el: Failed to byte compile '%s'" file-el-path)))))
    (require 'settings (file-name-sans-extension file-el-path))))

;; For now, do not byte compile the settings files. For some reason, this
;; messes up some of the use-package declaration.
(let ((main-settings (expand-file-name "settings.org" user-emacs-directory))
      (local-settings (expand-file-name "settings-local.org" user-emacs-directory)))
  (+org-babel-load-file main-settings)
  (when (file-exists-p local-settings)
    (+org-babel-load-file local-settings)))

;;; init.el ends here
