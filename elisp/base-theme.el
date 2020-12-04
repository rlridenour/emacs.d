(use-package hc-zenburn-theme
  :config (load-theme 'hc-zenburn t))




(setq default-frame-alist '((font . "Droid Sans Mono Slashed-16"))) ;;; set default font for emacs --daemon / emacsclient
(add-to-list 'default-frame-alist '(width  . (text-pixels . 940)))
(add-to-list 'default-frame-alist '(height . (text-pixels . 1200)))

;; Change background color for selected text to make it easier to see.
(set-face-attribute 'region nil :background "#666")

(provide 'base-theme)
;;; base-theme ends here

