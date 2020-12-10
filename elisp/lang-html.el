(use-package web-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(setq web-mode-engines-alist
		  '(("django"    . "\\.html\\'")))

(setq web-mode-ac-sources-alist
		  '(("css" . (ac-source-css-property))
			  ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-quoting t)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(use-package emmet-mode
:diminish (emmet-mode . "ε")
:bind* (("C-)" . emmet-next-edit-point)
			  ("C-(" . emmet-prev-edit-point))
:commands (emmet-mode
			     emmet-next-edit-point
			     emmet-prev-edit-point)
:init
(setq emmet-indentation 4)
(setq emmet-move-cursor-between-quotes t)
:config
;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'emmet-mode)
(setq emmet-move-cursor-between-quotes t) ;; default nil
)


(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)

(use-package company-web
	:config
	(require 'company-web-html))


(add-hook 'web-mode-hook (lambda ()
							             (set (make-local-variable 'company-backends) '(company-web-html))
							             (company-mode t)))

(defun my-web-mode-hook ()
(set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files))
)


(add-hook 'web-mode-hook  'emmet-mode) 

(add-hook 'web-mode-before-auto-complete-hooks
			    '(lambda ()
			       (let ((web-mode-cur-language
					          (web-mode-language-at-pos)))
				       (if (string= web-mode-cur-language "php")
					         (yas-activate-extra-mode 'php-mode)
				         (yas-deactivate-extra-mode 'php-mode))
				       (if (string= web-mode-cur-language "css")
					         (setq emmet-use-css-transform t)
				         (setq emmet-use-css-transform nil)))))

(provide 'lang-html)
