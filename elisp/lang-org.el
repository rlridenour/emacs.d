(use-package org
  :straight org-plus-contrib)

(use-package org
  :init
  (setq org-directory "~/Dropbox/org/")
  :config
  (setq org-startup-indented nil)
  (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars nil))


(require 'org-tempo)


;; Enable ignoring a headline during export.
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))


(require 'ox-latex)

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
			 '("rlr-article"
			   "\\documentclass{article}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
			   ("\\section{%s}" . "\\section*{%s}")
			   ("\\subsection{%s}" . "\\subsection*{%s}")
			   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			   ("\\paragraph{%s}" . "\\paragraph*{%s}")
			   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
)

;; (add-hook 'org-mode-hook 'wc-mode)


(defun flyspell-ignore-tex ()
	(interactive)
	(set (make-variable-buffer-local 'ispell-parser) 'tex))
(add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))
(add-hook 'org-mode-hook 'flyspell-ignore-tex)

(use-package org-ref
  :after org
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite
	org-ref-default-bibliography '("~/bibtex/rlr-bib/rlr.bib")))


;; Return adds new heading or list item. From https://github.com/aaronjensen/emacs-orgonomic
(use-package orgonomic
  :defer t
  :straight (orgonomic :host github :repo "aaronjensen/emacs-orgonomic")
  :hook (org-mode . orgonomic-mode))

;; Functions for automating lecture notes and slides


(defun lecture-slides ()
  "publish org file as beamer slides and notes"
  (interactive)
  (find-file "*-slides.org" t)
  (org-beamer-export-to-latex)
  (kill-buffer)
  (find-file "*-notes.org" t)
  (org-beamer-export-to-latex)
  (kill-buffer)
					;(kill-buffer "*.tex")
  (find-file "*-data.org" t))



(provide 'lang-org)
