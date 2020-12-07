(use-package counsel
  :diminish ivy-mode
  :bind
  (("s-r" . counsel-recentf)
   ("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch-backward)
   ("s-f" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("s-o" . counsel-find-file)
   ("C-c b" . counsel-bookmark)
   ("C-c i" . counsel-imenu)
   ("s-." . ivy-switch-buffer)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-load-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c s" . counsel-rg)
   ("C-x l" . locate)
   ("C-S-o" . counsel-rhythmbox)
   ("C-c C-r" . ivy-resume)
   ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line-and-call))
  :init
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq counsel-find-file-ignore-regexp
	(concat
	 ;; File names beginning with # or .
	 "\\(?:\\`[#.]\\)"
	 ;; File names ending with # or ~
	 "\\|\\(?:\\`.+?[#~]\\'\\)"))
  :config
  (ivy-mode 1)
  (counsel-mode 1))

(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

(use-package  ace-window
  :ensure
  :bind ("s-w" . ace-window)
  :config
  ;; (setq aw-leading-char-style 'path)
  (setq aw-background nil)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package shrink-whitespace
  :bind ("M-=" . shrink-whitespace))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck)

(use-package smex)

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (global-auto-revert-mode))

(use-package reveal-in-osx-finder
  :bind ("C-c z" . reveal-in-osx-finder))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))

(use-package smartparens
  :diminish smartparens-mode)
(require 'smartparens-config)
(smartparens-global-mode t)

(use-package aggressive-indent)

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind (("s-z" . undo-tree-undo)
	 ("s-Z" . undo-tree-redo)))

(use-package evil-nerd-commenter
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)))

(use-package which-key
  :config
  (which-key-mode))

(use-package windmove
  :bind
  ("C-x <up>" . windmove-up)
  ("C-x <down>" . windmove-down)
  ("C-x <left>" . windmove-left)
  ("C-x <right>" . windmove-right))

(use-package wgrep)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
        '("~/.emacs.d/elisp/snippets")))
  (yas-global-mode 1))

(use-package wc-mode)

(provide 'base-extensions)
