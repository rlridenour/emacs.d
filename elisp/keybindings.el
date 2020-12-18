;; Keybindings

(global-unset-key (kbd "C-z"))
;; (global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-m"))

(use-package key-chord
  :defer t
  :config
  (key-chord-mode 1))


;; Hydras


;; (use-package ivy-hydra)

(use-package hydra)

(use-package major-mode-hydra
  :bind
  ("s-m" . major-mode-hydra))



;; Hydra-toggle


(pretty-hydra-define hydra-toggle
  (:color blue :quit-key "q" :title "Toggle")
  ("Basic"
   (("a" abbrev-mode "abbrev" :toggle t)
    ("d" toggle-debug-on-error "debug" (default value 'debug-on-error))
    ("i" aggressive-indent-mode "indent" :toggle t)
    ("f" auto-fill-mode "fill" :toggle t)
    ("l" display-line-numbers-mode "linum" :toggle t)
    ("m" toggle-frame-maximized-undecorated "max" :toggle t)
    ("p" smartparens-mode "smartparens" :toggle t)
    ("t" toggle-truncate-lines "truncate" :toggle t)
    ("s" whitespace-mode "whitespace" :toggle t))
   "Writing"
   (("c" cdlatex-mode "cdlatex" :toggle t)
    ("o" olivetti-mode "olivetti" :toggle t)
    ("r" read-only-mode "read-only" :toggle t)
    ("w" wc-mode "word-count" :toggle t))))




;; (global-set-key (kbd "s-t") 'hydra-toggle/body)

;; Major-mode Hydras


(major-mode-hydra-define markdown-mode
  (:quit-key "q")
  ("Format"
   (("h" markdown-insert-header-dwim "header")
    ("l" markdown-insert-link "link")
    ("u" markdown-insert-uri "url")
    ("f" markdown-insert-footnote "footnote")
    ("w" markdown-insert-wiki-link "wiki")
    ("r" markdown-insert-reference-link-dwim "r-link")
    ("n" markdown-cleanup-list-numbers "clean-lists")
    ("c" markdown-complete-buffer "complete"))))


(major-mode-hydra-define latex-mode
  (:quit-key "q")
  ("Bibtex"
   (("b" ivy-bibtex "Ivy-Bibtex"))
   "LaTeXmk"
   (("p" rlr/tex-pvc "pvc")
    ("c" tex-clean "clean aux")
    ("C" tex-clean-all "clean all"))))

(major-mode-hydra-define org-mode
  (:quit-key "q")
  ("Export"
   (("l" org-latex-export-to-latex "Org to LaTeX")
    ("p" org-latex-export-to-pdf "Org to PDF")
    ("b" org-beamer-export-to-pdf "Org to Beamer-PDF")
    ("B" org-beamer-export-to-latex "Org to Beamer-LaTeX")
    ("s" lecture-slides "Lecture slides")
    )
   "Bibtex"
   (("r" ivy-bibtex "Ivy-Bibtex"))
   "View"
   (("p" org-toggle-pretty-entities "org-pretty"))
   "Clean"
   (("c" tex-clean "clean aux")
    ("C" tex-clean-all "clean all"))))




(major-mode-hydra-define dired-mode
  (:quit-key "q")
  ("Tools"
   (("d" +macos/open-in-default-program "Open in Default Program"))))



(defhydra hydra-locate (:color blue)
  ("l" avy-goto-line "avy-line")
  ("L" goto-line "goto-line")
  ("w" avy-goto-word-1 "goto-word")
  ("b" ivy-bookmark-goto "bookmarks")
  ("m" counsel-imenu "imenu")
  ("q" nil))




(defhydra hydra-org (:color blue)
  ("a" org-agenda "agenda")
  ("l" org-store-link "store-link")
  ("q" nil))




(bind-chords
 ("jh" . prelude-switch-to-previous-buffer)
 ("hj" . prelude-switch-to-previous-buffer))



;; Global Keybindings

;; Make things more Mac-like

(general-define-key
 "<s-up>" 'beginning-of-buffer
 "<s-down>" 'end-of-buffer
 "<s-right>" 'end-of-visual-line
 "<s-left>" 'beginning-of-visual-line
 "s-w" 'delete-frame
 "<C-tab>" 'other-window
 "<M-down>" 'forward-paragraph
 "<M-up>" 'backward-paragraph)




(general-define-key

 ;; Windows and frames
 "C-0" 'delete-window-balance
 "C-1" 'delete-other-windows
 "C-2" 'split-window-below-focus
 "C-3" 'split-window-right-focus
 "C-4" 'nuke-all-buffers
 "s-6" 'toggle-window-split
 "S-C-<left>" 'shrink-window-horizontally
 "S-C-<right>" 'enlarge-window-horizontally
 "S-C-<down>" 'shrink-window
 "S-C-<up>" 'enlarge-window
 "C-x w" 'delete-frame
 "M-o" 'crux-other-window-or-switch-buffer

 ;; Files and buffers
 "C-x c" 'save-buffers-kill-emacs
 "C-x C-b" 'ibuffer
 "C-`" 'iterm-goto-filedir-or-home
 "s-o" 'consult-buffer
 "s-k" 'kill-this-buffer
 "s-r" 'consult-buffer

 ;; Search

 "s-l" 'hydra-locate/body
 "s-f" 'consult-line

 ;; Editing
 "RET" 'newline-and-indent
 "M-/" 'hippie-expand
 "C-+" 'text-scale-increase
 "C--" 'text-scale-decrease
 "<s-backspace>" 'kill-whole-line
 "s-j" 'crux-top-join-line
 "C-k" 'crux-smart-kill-line
 "<S-return>" 'crux-smart-open-line
 "<C-S-return>" 'crux-smart-open-line-above
 "M-y" 'consult-yank-pop

 "s-t" 'hydra-toggle/body

 "s-/" 'avy-goto-char-timer
 "s-d" 'diredp-dired-recent-dirs
 "s-=" 'endless/ispell-word-then-abbrev
 "<help> a" 'consult-apropos
 "C-x 4 b" 'consult-buffer-other-window
 "C-x 5 b" 'consult-buffer-other-frame
 "C-x r x" 'consult-register
 "M-s m" 'consult-multi-occur
 "<f8>" 'insert-standard-date)


;; "C-c u" 'unfill-paragraph
;; "C-c C-<return>" 'split-org-item)
;; "C-c o" 'crux-open-with
;; "C-c D" 'crux-delete-file-and-buffer
;; "C-c C-k" 'compile



;; * Prefix Keybindings
;; :prefix can be used to prevent redundant specification of prefix keys
(general-define-key
 :prefix "C-c"
 ;; bind "C-c a" to 'org-agenda
 "a" 'org-agenda
 "b" 'consult-bookmark
 "c" 'org-capture
 "D" 'crux-delete-file-and-buffer
 "h" 'consult-history
 "k" 'crux-kill-other-buffers
 "m" 'consult-mark
 "o" 'consult-outline
 "r" 'crux-rename-file-and-buffer
 "s" 'crux-cleanup-buffer-or-region
 "t" 'crux-visit-term-buffer
 "u" 'unfill-paragraph
 "w" 'ace-window
 "z" 'reveal-in-osx-finder)
(provide 'keybindings)
