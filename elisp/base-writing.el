

;; Spelling

(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))

;; Store personal dictionary in Dropbox to sync between machines. Save to personal dictionary without asking.

(setq ispell-personal-dictionary "/Users/rlridenour/Dropbox/emacs/spelling/.aspell.en.pws")
(setq ispell-silently-savep t)




;; From [[https://joelkuiper.eu/spellcheck_emacs][Joel Kuiper]]

;; Enable flyspell mode for highlighting spelling errors.

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; Check comments and strings when coding.
(dolist (mode '(emacs-lisp-mode-hook
		inferior-lisp-mode-hook
		clojure-mode-hook
		python-mode-hook
		js-mode-hook
		R-mode-hook))
  (add-hook mode
	    '(lambda ()
	       (flyspell-prog-mode))))

(setq flyspell-issue-welcome-flag nil)
(setq flyspell-issue-message-flag nil)


;; Use F7 to check the current word, M-F7 for the next word.

(global-set-key (kbd "<f7>") 'ispell-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f7>") 'flyspell-check-next-highlighted-word)



;; Flyspell-correct-ivy uses the Ivy interface for spelling suggestions.
(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map
	      ("s-;" . flyspell-correct-at-point)
	      ("C-;" . flyspell-correct-previous)))

(use-package olivetti)

(provide 'base-writing)
