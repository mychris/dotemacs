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

(when (not (member system-type '(gnu/linux windows-nt berkeley-unix)))
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

(when (and (version<= "28" emacs-version)
	   (boundp 'native-comp-debug))
  (custom-set-variables
   ;; Debug level for native compilation, a number between 0 and 3.
   '(native-comp-debug 0)
   ;; Optimization level for native compilation, a number between -1 and 3.
   ;; -1 functions are kept in bytecode form and no native compilation is performed.
   ;;  0 native compilation is performed with no optimizations.
   ;;  1 light optimizations.
   ;;  2 max optimization level fully adherent to the language semantic.
   ;;  3 max optimization level, to be used only when necessary.
   ;;    Warning: with 3, the compiler is free to perform dangerous optimizations.
   '(native-comp-speed 2)
   ;; Non-nil to prevent native-compiling of Emacs Lisp code.
   '(no-native-compile nil)
   ;; Non-nil means to natively compile packages as part of their installation.
   '(package-native-compile t)
   ;; Whether to report warnings and errors from asynchronous native compilation.
   '(native-comp-async-report-warnings-errors 'silent)))

;;;; Bootstrap package and use-package

(custom-set-variables
 '(network-security-level 'high)
 '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
 '(package-archives
   '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
     ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
     ("nongnu-elpa" . "https://elpa.nongnu.org/nongnu/")
     ("nongnu-elpa-devel" . "https://elpa.nongnu.org/nongnu-devel/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-archive-priorities
   '(("gnu-elpa" . 10)
     ("gnu-elpa-devel" . 9)
     ("nongnu-elpa" . 8)
     ("nongnu-elpa-devel" . 7)
     ("melpa-stable" . 6)
     ("melpa" . 5))))
(unless (fboundp 'package)
  (require 'package))
(custom-set-variables
 '(package-quickstart-file (expand-file-name "package-quickstart.el" user-emacs-cache-directory))
 '(package-quickstart t))

(if (file-exists-p package-quickstart-file)
    (package-activate-all)
  (package-initialize)
  (package-quickstart-refresh))

(let ((refreshed nil))
  (dolist (pack '(use-package general delight))
    (unless (package-installed-p pack)
      (unless refreshed
	(package-refresh-contents)
	(setq refreshed t))
      (package-install pack))
    (require pack)))

(eval-and-compile
  (custom-set-variables
   ;; Whether to report about loading and configuration details.
   ;;'(use-package-verbose (not (bound-and-true-p byte-compile-current-file)))
   '(use-package-verbose nil)
   ;; If non-nil, compute statistics concerned use-package declarations.
   ;;'(use-package-compute-statistics (not (bound-and-true-p byte-compile-current-file)))
   '(use-package-compute-statistics nil)
   ;; Minimal load timet that will be reported
   '(use-package-minimum-reported-time 0.001)
   ;; If non-nil, assume ':demand t' unless ':defer' is used.
   '(use-package-always-demand nil)
   ;; If non-nil, assume ':defer t' unless ':demand' is used.
   '(use-package-always-defer t)
   ;; Treat every package as though it had specified using ':pin SYM'
   '(use-package-always-pin nil)
   ;; Treat every package as though it had specified using ':ensure SEXP'
   '(use-package-always-ensure nil)
   ;; If non-nil, make the expanded code as minimal as possible.
   '(use-package-expand-minimally nil)
   ;; If non-nil, cause imenu to see 'use-package' declarations.
   '(use-package-enable-imenu-support t)
   ;; Text append to the name of hooks mentioned by :hook.
   '(use-package-hook-name-suffix nil))
  ;; Quit the use-package-report window and kill the buffer as well.
  (with-eval-after-load 'use-package
    (defvar use-package-statistics-mode-map)
    (define-key use-package-statistics-mode-map (kbd "q") #'kill-buffer-and-window)))

;;;; Load settings

(defun +org-babel-tangle-file-fast-sloppy (org-file target-file &optional lang-re)
  "Extract the bodies of source code blocks from ORG-FILE.
Do this in a fast and sloppy way, without loading `org'.  Only consider source
blocks which match LANG-RE.  Store the result to TARGET-FILE.

Only source code blocks are recognized, inline code blocks will be ignored.  In
addition, switches and other header arguments are not taken into account."
  (or lang-re (setq lang-re ".*"))
  (with-temp-buffer
    ;; read the file into the buffer
    (insert-file-contents org-file)
    (goto-char (point-min))
    ;; Delete everything from the buffer, which is not between begin_src and end_src
    (let ((start (point))
	  (count 0))
      (while (progn
	       (when (re-search-forward (concat "^#\\+begin_src[[:space:]]+" lang-re) nil t)
		 (end-of-line)
		 (delete-region start (point))
		 (re-search-forward "^#\\+end_src" nil t)))
	(setq count (1+ count))
	(beginning-of-line)
	(setq start (point)))
      (delete-region start (point-max))
      (message "Tangled %d code blocks from %s" (- count 1) org-file))
    ;; Remove the two spaces in front of every line
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (when (looking-at-p "^  ")
	(delete-char 2))
      (forward-line))
    ;; Remove the first empty line
    (goto-char (point-min))
    (when (looking-at-p "^$")
      (delete-char 1))
    ;; write the buffer to the target file
    (write-file target-file)))

(defun +file-time-less-p (a b)
  "Return non-nil if the file modification time for A is less than B."
  (time-less-p
   (file-attribute-modification-time (file-attributes a))
   (file-attribute-modification-time (file-attributes b))))

(defun +org-babel-load-file (file-org-path)
  "Load the given FILE-ORG-PATH using `org-bable-load-file'.
The file is also byte-compiled, if DO-BYTE-COMPILE evaluates to a non-nil
value."
  (let* ((file-el-name (concat (file-name-base file-org-path) ".el"))
	 (file-el-path (expand-file-name file-el-name user-emacs-cache-directory)))
    (if (not (file-exists-p file-org-path))
	(error "init.el: Org file '%s' not found" file-org-path)
      (when (not (and (file-exists-p file-el-path)
		      (+file-time-less-p file-org-path file-el-path)))
	;;(require 'org)
	;;(org-babel-tangle-file file-org-path file-el-path "emacs-lisp")
	(+org-babel-tangle-file-fast-sloppy file-org-path file-el-path "emacs-lisp")))
    (when (not (load-file file-el-path))
      (error "init.el: Failed to load '%s'" (file-name-sans-extension file-el-path)))))

;; `file-name-handler-alist' is very expensive.
;; With many packages installed, I counted more than 100.000 calls to
;; `find-file-name-handler' during startup.
;; emacs -Q only had around 1.500 calls.
;; Setting `file-name-handler-alist' to `nil' during startup saves time.
;;
;; This will be a problem, if during the init phase, some package needs a file
;; handler. This can happen in very obscure ways.
;; For instance, when `saveplace' is loaded, it loads its cache from disk, goes
;; through all the files it has stored and calls some function on the path.
;; If a file in the cache needs a file handler (it referes to some ssh or sudo
;; tramp path, for instance), the behavior will change, if
;; `file-name-handler-alist' does not contain the tramp handler.
;;
;; To avoid this, try to load all packages in `after-init-hook'.
;;
;; Set `remove-file-name-handler-alist-during-startup' to nil if something
;; weird happens.
(let ((backup-file-name-handler-alist file-name-handler-alist)
      (remove-file-name-handler-alist-during-startup t))
  (when remove-file-name-handler-alist-during-startup
    (setq file-name-handler-alist nil))
  ;; For now, do not byte compile the settings files. For some reason, this
  ;; messes up some of the use-package declaration.
  (let ((main-settings (expand-file-name "settings.org" user-emacs-directory))
	(local-settings (expand-file-name "settings-local.org" user-emacs-directory)))
    (+org-babel-load-file main-settings)
    (when (file-exists-p local-settings)
      (+org-babel-load-file local-settings)))
  (when remove-file-name-handler-alist-during-startup
    (dolist (e file-name-handler-alist)
      (push e backup-file-name-handler-alist))
    (setq file-name-handler-alist backup-file-name-handler-alist)))

(fmakunbound '+org-babel-tangle-file-fast-sloppy)
(fmakunbound '+org-babel-load-file)
(fmakunbound '+file-time-less-p)

;;; init.el ends here
