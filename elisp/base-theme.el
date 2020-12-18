(use-package hc-zenburn-theme)

(mapc #'disable-theme custom-enabled-themes)

(use-package modus-themes
  :straight (:host gitlab :repo "protesilaos/modus-themes" :branch "main")
  :config
  (setq modus-themes-slanted-constructs t
      modus-themes-bold-constructs t
      modus-themes-fringes 'intense ; {nil,'subtle,'intense}
      modus-themes-mode-line '3d ; {nil,'3d,'moody}
      modus-themes-syntax 'faint ; Lots of options---continue reading the manual
      modus-themes-intense-hl-line nil
      modus-themes-paren-match 'subtle-bold ; {nil,'subtle-bold,'intense,'intense-bold}
      modus-themes-links 'faint-neutral-underline ; Lots of options---continue reading the manual
      modus-themes-no-mixed-fonts t
      modus-themes-prompts 'intense ; {nil,'subtle,'intense}
      modus-themes-completions 'opinionated ; {nil,'moderate,'opinionated}
      modus-themes-region 'bg-only-no-extend ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
      modus-themes-diffs nil ; {nil,'desaturated,'fg-only,'bg-only}
      modus-themes-org-blocks nil ; {nil,'grayscale,'rainbow}
      modus-themes-variable-pitch-headings nil
      modus-themes-scale-headings t
      modus-themes-scale-1 1.1
      modus-themes-scale-2 1.15
      modus-themes-scale-3 1.21
      modus-themes-scale-4 1.27
      modus-themes-scale-5 1.33))

(load-theme 'modus-operandi t)



(setq frame-resize-pixelwise t)

;; (setq default-frame-alist '((font . "Droid Sans Mono Slashed-16"))) ;;; set default font for emacs --daemon / emacsclient

;; Main typeface
(set-face-attribute 'default nil :family "Droid Sans Mono Slashed" :height 160)

;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "Droid Sans" :height 1.0)

;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono Slashed" :height 1.0)


(add-to-list 'default-frame-alist '(fullscreen . fullheight))


;; Change background color for selected text to make it easier to see.
;; (set-face-attribute 'region nil :background "#666")

(provide 'base-theme)
;;; base-theme ends here

