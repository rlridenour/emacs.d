
(use-package general
  :config
  (general-auto-unbind-keys)
  )
(use-package flycheck)

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package prescient)

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))



(use-package consult
  :straight (:host github :repo "minad/consult" :branch "main")
  ;; Replace bindings. Lazily loaded due to use-package.
         

  ;; The :init configuration is always executed (Not lazy!)
  :init
  (setq consult-themes '(modus-operandi modus-vivendi hc-zenburn))

  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur))

(use-package consult-selectrum
  :straight (:host github :repo "minad/consult" :branch "main")
  )
         
(use-package marginalia
  :straight (:host github :repo "minad/marginalia" :branch "main")
  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :straight (:host github :repo "oantolin/embark" :branch "master")
  :after selectrum
  :bind (:map minibuffer-local-map
         ("C-o" . embark-act)
         ("C-S-o" . embark-act-noexit)
         :map embark-file-map
         ("j" . dired-jump)))




(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

(use-package  ace-window
  :bind ("C-c w" . ace-window)
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
  ;; :bind (("s-z" . undo-tree-undo)
	 ;; ("s-Z" . undo-tree-redo)))
  :general
  ("s-z" 'undo-tree-undo
   "s-Z" 'undo-tree-redo))

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


(use-package company-prescient
	:config
	(company-prescient-mode t))

(use-package crux)

(provide 'base-extensions)
