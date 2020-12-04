;; Keybindings

(global-unset-key (kbd "C-z"))
;; (global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-m"))

(use-package key-chord
  :defer t
  :config
  (key-chord-mode 1))


;; Hydras


(use-package ivy-hydra)

(use-package major-mode-hydra
  :bind
  ("s-m" . major-mode-hydra))



;; Hydra-toggle


(defhydra hydra-toggle (:color blue) 
  "toggle"
  ("a" abbrev-mode "abbrev")
  ("c" column-number-mode "column")
  ("C" cdlatex-mode "cdlatex")
  ("d" toggle-debug-on-error "debug")
  ("e" evil-mode "evil")
  ("f" auto-fill-mode "fill")
  ;; ("g" god-mode "god")
  ("l" display-line-numbers-mode "linum")
  ("m" toggle-frame-maximized-undecorated "max")
  ("o" olivetti-mode "olivetti")
  ("r" read-only-mode "read-only") 
  ("t" toggle-truncate-lines "truncate")
  ("w" wc-mode "word-count")
  ("W" whitespace-mode "whitespace")
  ("q" nil "global"))
;; (global-set-key (kbd "s-t") 'hydra-toggle/body)

;; Major-mode Hydras


(major-mode-hydra-bind markdown-mode "Format"
  ("h" markdown-insert-header-dwim "header") 
  ("l" markdown-insert-link "link")
  ("u" markdown-insert-uri "url")
  ("f" markdown-insert-footnote "footnote")
  ("w" markdown-insert-wiki-link "wiki")
  ("r" markdown-insert-reference-link-dwim "r-link")
  ("n" markdown-cleanup-list-numbers "clean-lists")
  ("c" markdown-complete-buffer "complete"))

(major-mode-hydra-bind markdown-mode "Pandoc"
		       ("A" pandoc-pdfarticle "pdfarticle")
		       ("B" pandoc-beamer "beamer")
		       ("C" pandoc-syllabus "Course syllabus")
		       ("S" pandoc-slides "slides")
		       ("H" pandoc-handout "handout")
		       ("O" pandoc-obuletter "obu letter")
		       ("D" pandoc-docx "docx")
		       ("H" pandoc-html "html")
		       ("P" pandoc-pdf "pdf")
		       ("t" pandoc-clean "trash non-md")

		       ("q" nil))

(major-mode-hydra-bind latex-mode "Bibtex"
		       ("b" ivy-bibtex "Ivy-Bibtex"))

(major-mode-hydra-bind latex-mode "LaTeXmk"
		       ("p" rlr/tex-pvc "pvc")
		       ("c" tex-clean "clean aux")
		       ("C" tex-clean-all "clean all")

		       ("q" nil))

(major-mode-hydra-bind org-mode "Export"
  ("b" org-beamer-export-to-pdf "Org to Beamer-PDF")
  ("p" org-latex-export-to-pdf "Org to PDF"))

(major-mode-hydra-bind org-mode "Bibtex"
  ("r" ivy-bibtex "Ivy-Bibtex"))

(major-mode-hydra-bind org-mode "Clean"
  ("c" tex-clean "clean aux")
  ("C" tex-clean-all "clean all")

  ("q" nil))



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




(bind-keys
 ("C-0" . delete-window-balance)
 ("C-1" . delete-other-windows)
 ("C-2" . split-window-below-focus)
 ("C-3" . split-window-right-focus)
 ("C-4" . nuke-all-buffers)
 ("s-5" . delete-frame)
 ("s-6" . toggle-window-split)
 ("S-C-<left>" . shrink-window-horizontally)
 ("S-C-<right>" . enlarge-window-horizontally)
 ("S-C-<down>" . shrink-window)
 ("S-C-<up>" . enlarge-window)
 ("C-x c" . save-buffers-kill-emacs)
 ("C-x w" . delete-frame)
 ;; ("s-." . helm-buffers-list)
 ;; ("C-c i" . ivy-imenu-goto)	
 ;; ("C-c b" . ivy-bookmark-goto)
 ("C-x C-b" . ibuffer)
 ("RET" . newline-and-indent)
 ("M-/" . hippie-expand)
 ("C-+" . text-scale-increase)
 ("C--" . text-scale-decrease)
 ("C-c C-k" . compile)
 ("<s-backspace>" . kill-whole-line)
 ("s-t" . hydra-toggle/body)
 ("s-\\" . hydra-org/body)
 ("s-l" . hydra-locate/body)
 ("C-c f" . hydra-locate/body)
 ("C-c k" . prelude-kill-other-buffers)
 ("C-c u" . unfill-paragraph)
 ("s-d" . rlr/ivy-dired-recent-dirs)
 ("C-c v" . counsel-M-x)
 ("s-=" . endless/ispell-word-then-abbrev)
 ("<f5>" . call-last-kbd-macro)
 ("C-c C-<return>" . split-org-item))

(provide 'keybindings)



