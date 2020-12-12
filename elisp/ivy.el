;; (use-package counsel
;;   :diminish ivy-mode
;;   :bind
;;   (("s-r" . counsel-recentf)
;;    ("C-s" . swiper-isearch)
;;    ("C-r" . swiper-isearch-backward)
;;    ("s-f" . swiper)
;;    ("M-x" . counsel-M-x)
;;    ("C-x C-f" . counsel-find-file)
;;    ("s-o" . counsel-find-file)
;;    ("C-c b" . counsel-bookmark)
;;    ("C-c i" . counsel-imenu)
;;    ("s-." . ivy-switch-buffer)
;;    ("<f1> f" . counsel-describe-function)
;;    ("<f1> v" . counsel-describe-variable)
;;    ("<f1> l" . counsel-load-library)
;;    ("<f2> i" . counsel-info-lookup-symbol)
;;    ("<f2> u" . counsel-unicode-char)
;;    ("C-c g" . counsel-git)
;;    ("C-c j" . counsel-git-grep)
;;    ("C-c s" . counsel-rg)
;;    ("C-x l" . locate)
;;    ("C-S-o" . counsel-rhythmbox)
;;    ("C-c C-r" . ivy-resume)
;;    ("M-y" . counsel-yank-pop)
;;    :map ivy-minibuffer-map
;;    ("M-y" . ivy-next-line-and-call))
;;   :init
;;   (setq ivy-display-style 'fancy)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-height 10)
;;   (setq ivy-count-format "(%d/%d) ")
;;   (setq counsel-find-file-ignore-regexp
;; 	(concat
;; 	 ;; File names beginning with # or .
;; 	 "\\(?:\\`[#.]\\)"
;; 	 ;; File names ending with # or ~
;; 	 "\\|\\(?:\\`.+?[#~]\\'\\)"))
;;   :config
;;   (ivy-mode 1)
;;   (counsel-mode 1))
