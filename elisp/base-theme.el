(use-package hc-zenburn-theme
  :config (load-theme 'hc-zenburn t))

(setq frame-resize-pixelwise t)

(setq default-frame-alist '((font . "Droid Sans Mono Slashed-16")
                            (height . fullheight)
                            (fullscreen-restore .fullheight)))

;; Change background color for selected text to make it easier to see.
(set-face-attribute 'region nil :background "#666")

(provide 'base-theme)
;;; base-theme ends here

