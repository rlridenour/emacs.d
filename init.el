;; This is the main init file for Randy Ridenour's Emacs configuration.

(setq user-full-name "Randy Ridenour")
(setq user-mail-address "rlridenour@gmail.com")

;; Code

;; Package management using straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Replace use-package with straight-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;  Ensure that system utilities required by various packages are installed.
(use-package use-package-ensure-system-package)

;; Allow key chords in use-package bindings.
  (use-package use-package-chords
	:config (key-chord-mode 1))

;; Suppress ad-handle definition warnings
;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)


;; Set the path variable to match the shell.


(use-package exec-path-from-shell
	:config (exec-path-from-shell-initialize))


;; Add Homebrew packages to the path.

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
	(normal-top-level-add-subdirs-to-load-path))

;; set load path
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'base)
(require 'base-theme)
(require 'base-extensions)
(require 'base-functions)
(require 'base-writing)
(require 'base-dired)
(require 'lang-org)
(require 'lang-markdown)
(require 'lang-latex)
(require 'lang-html)
(require 'keybindings)
(require 'misc)

(provide 'init)

;;; init.el ends here
(put 'upcase-region 'disabled nil)
