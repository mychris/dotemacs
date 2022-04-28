;;; custom.el --- customizations                     -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-background nil nil nil "Customized with use-package ace-window")
 '(aw-keys '(97 115 100 102 103 104 106 107 108) nil nil "Customized with use-package ace-window")
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "57fe2bf84d81baecc6d89ed97bdb19936a3052fc2551ca178667fc45feef2381" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" default))
 '(golden-ratio-scroll-highlight-flag nil nil nil "Do not highlight current line before/after scroll")
 '(helm-minibuffer-history-key "M-p")
 '(highlight-thing-delay-seconds 2)
 '(highlight-thing-exclude-thing-under-point t)
 '(highlight-thing-prefer-active-region t)
 '(ivy-display-style 'fancy)
 '(ivy-use-virtual-buffers t)
 '(lsp-ui-peek-enable t)
 '(lsp-ui-sideline-enable nil)
 '(my-background-color "gray9" t)
 '(neo-dont-be-alone t t)
 '(neo-smart-open t)
 '(neo-theme 'icons)
 '(neo-vc-integration '(face char))
 '(neo-window-fixed-size nil)
 '(org-bullets-bullet-list '("◉" "○" "✸" "✿" "☼" "⚬") t)
 '(package-selected-packages
   '(embark s telephone-line use-package rustic dune utop merlin tuareg evil-org-agenda golden-ration treemacs-persp treemacs-magit treemacs-projectile treemacs string-inflection string-infliction helm-gtags helm-projectile projectile jenkinsfile-mode groovy-mode fzf monky gitignore-mode magit org-superstar elpy python-mode company-shell company keycast help xah-fly-keys embark-consult all-the-icons spinner ggtags spacemacs-theme lsp-ui company-lsp cquery lsp-mode flycheck-pos-tip json-mode yaml-mode markdown-mode delight company-quickhelp toml-mode racer smart-mode-line sexy-monochrome org-bullets expand-region yasnippet counsel ace-window which-key zenburn-theme flycheck-rust flycheck cargo undo-tree neotree))
 '(sml/mode-width 'full)
 '(sml/name-width 30)
 '(sml/no-confirm-load-theme t)
 '(sml/shorten-modes t)
 '(sml/theme 'dark)
 '(undo-tree-auto-save-history t)
 '(undo-tree-visualizer-diff t))

;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
