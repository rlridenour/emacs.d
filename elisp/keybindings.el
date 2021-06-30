;; Keybindings

(global-unset-key (kbd "C-z"))
;; (global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-h"))

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

(defun my/insert-unicode (unicode-name)
       "Same as C-x 8 enter UNICODE-NAME."
       (insert-char (gethash unicode-name (ucs-names))))

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



(pretty-hydra-define hydra-logic
  (:color blue :title "Logic")
  ("Operators"
   (("1" (my/insert-unicode "NOT SIGN") "¬" :exit nil)
    ("2" (my/insert-unicode "AMPERSAND") "&" :exit nil)
    ("3" (my/insert-unicode "LOGICAL OR") "v" :exit nil)
    ("4" (my/insert-unicode "RIGHTWARDS ARROW") "→" :exit nil)
    ("5" (my/insert-unicode "LEFT RIGHT ARROW") "↔" :exit nil)
    ("6" (my/insert-unicode "THERE EXISTS") "∃" :exit nil)
    ("7" (my/insert-unicode "FOR ALL") "∀" :exit nil)
    ("8" (my/insert-unicode "WHITE MEDIUM SQUARE") "□" :exit nil)
    ("9" (my/insert-unicode "LOZENGE") "◊" :exit nil))
   "Lowercase"
   (("a" (my/insert-unicode "LATIN SMALL LETTER A") "a" :exit nil)
    ("b" (my/insert-unicode "LATIN SMALL LETTER B") "b" :exit nil)
    ("c" (my/insert-unicode "LATIN SMALL LETTER C") "c" :exit nil)
    ("d" (my/insert-unicode "LATIN SMALL LETTER D") "d" :exit nil)
    ("e" (my/insert-unicode "LATIN SMALL LETTER E") "e" :exit nil)
    ("f" (my/insert-unicode "LATIN SMALL LETTER F") "f" :exit nil)
    ("g" (my/insert-unicode "LATIN SMALL LETTER G") "g" :exit nil)
    ("h" (my/insert-unicode "LATIN SMALL LETTER H") "h" :exit nil)
    ("i" (my/insert-unicode "LATIN SMALL LETTER I") "i" :exit nil)
    ("j" (my/insert-unicode "LATIN SMALL LETTER J") "j" :exit nil)
    ("k" (my/insert-unicode "LATIN SMALL LETTER K") "k" :exit nil)
    ("l" (my/insert-unicode "LATIN SMALL LETTER L") "l" :exit nil)
    ("m" (my/insert-unicode "LATIN SMALL LETTER M") "m" :exit nil)
    ("n" (my/insert-unicode "LATIN SMALL LETTER N") "n" :exit nil)
    ("o" (my/insert-unicode "LATIN SMALL LETTER O") "o" :exit nil)
    ("p" (my/insert-unicode "LATIN SMALL LETTER P") "p" :exit nil)
    ("q" (my/insert-unicode "LATIN SMALL LETTER Q") "q" :exit nil)
    ("r" (my/insert-unicode "LATIN SMALL LETTER R") "r" :exit nil)
    ("s" (my/insert-unicode "LATIN SMALL LETTER S") "s" :exit nil)
    ("t" (my/insert-unicode "LATIN SMALL LETTER T") "t" :exit nil)
    ("u" (my/insert-unicode "LATIN SMALL LETTER U") "u" :exit nil)
    ("v" (my/insert-unicode "LATIN SMALL LETTER V") "v" :exit nil)
    ("w" (my/insert-unicode "LATIN SMALL LETTER W") "w" :exit nil)
    ("x" (my/insert-unicode "LATIN SMALL LETTER X") "x" :exit nil)
    ("y" (my/insert-unicode "LATIN SMALL LETTER Y") "y" :exit nil)
    ("z" (my/insert-unicode "LATIN SMALL LETTER Z") "z" :exit nil))
   "Uppercase"
   (("A" (my/insert-unicode "LATIN CAPITAL LETTER A") "A" :exit nil)
    ("B" (my/insert-unicode "LATIN CAPITAL LETTER B") "B" :exit nil)
    ("C" (my/insert-unicode "LATIN CAPITAL LETTER C") "C" :exit nil)
    ("D" (my/insert-unicode "LATIN CAPITAL LETTER D") "D" :exit nil)
    ("E" (my/insert-unicode "LATIN CAPITAL LETTER E") "E" :exit nil)
    ("F" (my/insert-unicode "LATIN CAPITAL LETTER F") "F" :exit nil)
    ("G" (my/insert-unicode "LATIN CAPITAL LETTER G") "G" :exit nil)
    ("H" (my/insert-unicode "LATIN CAPITAL LETTER H") "H" :exit nil)
    ("I" (my/insert-unicode "LATIN CAPITAL LETTER I") "I" :exit nil)
    ("J" (my/insert-unicode "LATIN CAPITAL LETTER J") "J" :exit nil)
    ("K" (my/insert-unicode "LATIN CAPITAL LETTER K") "K" :exit nil)
    ("L" (my/insert-unicode "LATIN CAPITAL LETTER L") "L" :exit nil)
    ("M" (my/insert-unicode "LATIN CAPITAL LETTER M") "M" :exit nil)
    ("N" (my/insert-unicode "LATIN CAPITAL LETTER N") "N" :exit nil)
    ("O" (my/insert-unicode "LATIN CAPITAL LETTER O") "O" :exit nil)
    ("P" (my/insert-unicode "LATIN CAPITAL LETTER P") "P" :exit nil)
    ("Q" (my/insert-unicode "LATIN CAPITAL LETTER Q") "Q" :exit nil)
    ("R" (my/insert-unicode "LATIN CAPITAL LETTER R") "R" :exit nil)
    ("S" (my/insert-unicode "LATIN CAPITAL LETTER S") "S" :exit nil)
    ("T" (my/insert-unicode "LATIN CAPITAL LETTER T") "T" :exit nil)
    ("U" (my/insert-unicode "LATIN CAPITAL LETTER U") "U" :exit nil)
    ("V" (my/insert-unicode "LATIN CAPITAL LETTER V") "V" :exit nil)
    ("W" (my/insert-unicode "LATIN CAPITAL LETTER W") "W" :exit nil)
    ("X" (my/insert-unicode "LATIN CAPITAL LETTER X") "X" :exit nil)
    ("Y" (my/insert-unicode "LATIN CAPITAL LETTER Y") "Y" :exit nil)
    ("Z" (my/insert-unicode "LATIN CAPITAL LETTER Z") "Z" :exit nil))
   "Parens"
   (("(" (my/insert-unicode "LEFT PARENTHESIS") "(" :exit nil)
    (")" (my/insert-unicode "RIGHT PARENTHESIS") ")" :exit nil)
    ("[" (my/insert-unicode "LEFT SQUARE BRACKET") "[" :exit nil)
    ("]" (my/insert-unicode "RIGHT SQUARE BRACKET") "]" :exit nil)
    ("{" (my/insert-unicode "LEFT CURLY BRACKET") "{" :exit nil)
    ("}" (my/insert-unicode "RIGHT CURLY BRACKET") "}" :exit nil))
   "Space"
   (("<SPC>" (my/insert-unicode "SPACE") "Space" :exit nil)
    ("?" (my/insert-unicode "MEDIUM MATHEMATICAL SPACE") "Narrow space" :exit nil)
    ("<left>" backward-char "move-left" :exit nil)
    ("<right>" forward-char "move-right" :exit nil)
    ("<kp-delete>" delete-char "delete" :exit nil))))

;; Hydra-hugo

(pretty-hydra-define hydra-hugo
  (:color blue :quit-key "q" :title "Hugo")
  ("Blog"
   (("n" hugo-draft-post "new draft")
    ("p" hugo-publish-post "publish post")
    ("t" hugo-timestamp "update timestamp"))))



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
   (("d" crux-open-with "Open in Default Program"))))



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
 "s-o" 'find-file
 "s-k" 'kill-this-buffer
 "s-r" 'consult-buffer
 "C-S-a" 'embark-act

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
 "s-h" 'hydra-hugo/body
 "C-x 9" 'hydra-logic/body

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
 ;; "h" 'consult-history
 "k" 'crux-kill-other-buffers
 "m" 'consult-mark
 "o" 'consult-outline
 "r" 'crux-rename-file-and-buffer
 "s" 'goto-scratch
 "S" 'crux-cleanup-buffer-or-region
 "t" 'crux-visit-term-buffer
 "u" 'unfill-paragraph
 "w" 'ace-window
 "z" 'reveal-in-osx-finder)
(provide 'keybindings)
