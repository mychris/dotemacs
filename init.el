;;; init.el --- Emacs configuration file

;;; Commentary:

;; Emacs configuration file.
;; Does some basic setup (primarily for use-package)
;; and then loads the custom settings from the settings.org mode file.
;; Do not edit settings.el manually.
;; The file is generated from the settings.org file, if required.

;;; Code:

;;;; Customization information

(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p custom-file)
	(load custom-file)))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-background nil t)
 '(aw-keys '(97 115 100 102 103 104 106 107 108) t)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "57fe2bf84d81baecc6d89ed97bdb19936a3052fc2551ca178667fc45feef2381" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" default))
 '(golden-ratio-scroll-highlight-flag nil t)
 '(highlight-thing-delay-seconds 2)
 '(highlight-thing-exclude-thing-under-point t)
 '(highlight-thing-prefer-active-region t)
 '(ivy-display-style 'fancy)
 '(ivy-use-virtual-buffers t)
 '(lsp-ui-peek-enable t t)
 '(lsp-ui-sideline-enable nil t)
 '(my-background-color "gray9" t)
 '(neo-dont-be-alone t t)
 '(neo-smart-open t)
 '(neo-theme 'icons)
 '(neo-vc-integration '(face char))
 '(neo-window-fixed-size nil)
 '(org-bullets-bullet-list '("◉" "○" "✸" "✿" "☼" "⚬") t)
 '(package-selected-packages
   '(embark marginalia consult diminish org-superstar magit keycast all-the-icons spinner ggtags spacemacs-theme lsp-ui company-lsp cquery lsp-mode flycheck-pos-tip json-mode yaml-mode markdown-mode delight company-quickhelp toml-mode racer sexy-monochrome expand-region yasnippet-snippets yasnippet counsel ace-window which-key zenburn-theme flycheck-rust flycheck cargo undo-tree rust-mode neotree))
 '(sml/mode-width 'full)
 '(sml/name-width 30)
 '(sml/no-confirm-load-theme t)
 '(sml/shorten-modes t)
 '(sml/theme 'dark)
 '(tab-always-indent 'complete)
 '(undo-tree-auto-save-history t)
 '(undo-tree-visualizer-diff t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
