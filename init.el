;;; init.el --- Emacs configuration file

;;; Commentary:

;; Emacs configuration file.
;; Does some basic setup (primarily for use-package)
;; and then loads the custom settings from the settings.org mode file.
;; Do not edit settings.el manually.
;; The file is generated from the settings.org file, if required.

;;; Code:

;;;; Customization information

;; increase this early, decrease later on again
;; define it in init and early-init for compatibility
(setq gc-cons-threshold (* 200 1000 1000))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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

;;;; Bootstrap use-package

;; this should already be loaded, but I got an error in emacs-27.1 for some reason.
;; TODO: investigate
(unless (boundp 'package-archives)
  (require 'package))
(setq-default gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq-default package-quickstart-file (expand-file-name "package-quickstart.el" user-emacs-cache-directory))
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
  ;; If non-nil, assume ':demand t' unless ':defer' is used.
  (setq-default use-package-always-demand nil)
  ;; If non-nil, assume ':defer t' unless ':demand' is used.
  (setq-default use-package-always-defer nil)
  ;; Treat every package as though it had specified using ':pin SYM'
  (setq-default use-package-always-pin nil)
  ;; Treat every package as though it had specified using ':ensure SEXP'
  (setq-default use-package-always-ensure nil)
  ;; If non-nil, compute statistics concerned use-package declarations.
  (setq-default use-package-compute-statistics nil)
  ;; If non-nil, make the expanded code as minimal as possible.
  (setq-default use-package-expand-minimally nil)
  ;; If non-nil, cause imenu to see 'use-package' declarations.
  (setq-default use-package-enable-imenu-support t)
  ;; Text append to the name of hooks mentioned by :hook.
  (setq-default use-package-hook-name-suffix nil)
  (require 'use-package))

(eval-when-compile
  (require 'cl-lib))
(require 'diminish)
(require 'delight)
(require 'bind-key)

;;;; Load settings

(add-to-list 'load-path (file-name-as-directory (expand-file-name "contrib" user-emacs-directory)))

;; only load the org file if it is newer than the el file.
;; Compile the el file after loading it from the org file.
(let* ((settings-org (expand-file-name "settings.org" user-emacs-directory))
       (settings-el (concat (file-name-sans-extension settings-org) ".el")))
  (if (file-exists-p settings-org)
      (if (and (file-exists-p settings-el)
               (time-less-p
                (file-attribute-modification-time (file-attributes settings-org))
                (file-attribute-modification-time (file-attributes settings-el))))
          ;; found that one somewhere, shaves off another 200ms during startup.
          ;; no idea if this has any negative side effects, nothing emerged yet.
          (let ((file-name-handler-alist nil))
            (load-file settings-el))
        (progn
          (require 'org)
          (org-babel-load-file settings-org)
          (byte-compile-file settings-el)))
    (error "Init org file '%s' missing" settings-org)))

;;; init.el ends here
