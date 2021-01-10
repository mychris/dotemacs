;;; init.el --- Emacs configuration file

;;; Commentary:

;; Emacs configuration file.
;; Does some basic setup (primarily for use-package)
;; and then loads the custom settings from the settings.org mode file.
;; Do not edit settings.el manually.
;; The file is generated from the settings.org file, if required.

;;; Code:

;;;; Customization information

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;; Bootstrap use-package

(setq-default gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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
  (setq-default
   ;; If non-nil, assume ':demand t' unless ':defer' is used.
   use-package-always-demand nil
   ;; If non-nil, assume ':defer t' unless ':demand' is used.
   use-package-always-defer nil
   ;; Treat every package as though it had specified using ':pin SYM'
   use-package-always-pin nil
   ;; Treat every package as though it had specified using ':ensure SEXP'
   use-package-always-ensure nil
   ;; If non-nil, compute statistics concerned use-package declarations.
   use-package-compute-statistics nil
   ;; If non-nil, make the expanded code as minimal as possible.
   use-package-expand-minimally nil
   ;; If non-nil, cause imenu to see 'use-package' declarations.
   use-package-enable-imenu-support t
   ;; Text append to the name of hooks mentioned by :hook.
   use-package-hook-name-suffix nil
   )
  (require 'use-package))

(eval-when-compile
  (require 'cl-lib))
(require 'diminish)
(require 'delight)
(require 'bind-key)

;;;; Make sure some environment variables are set

(unless (getenv "XDG_CACHE_HOME")
	(setenv "XDG_CACHE_HOME" (expand-file-name "~/.cache")))

(unless (getenv "EMACS_CACHE_DIR")
	(setenv "EMACS_CACHE_DIR" (expand-file-name "emacs" (getenv "XDG_CACHE_HOME"))))

(unless (file-exists-p (getenv "EMACS_CACHE_DIR"))
  (make-directory (getenv "EMACS_CACHE_DIR")))

;;;; Load settings

(add-to-list 'load-path "~/.emacs.d/contrib/")

(require 'org)
(org-babel-load-file (expand-file-name "settings.org" user-emacs-directory))

;;; init.el ends here
