(use-package markdown-mode
  :defer t
  :mode (("\\.text\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.Rmd\\'" . markdown-mode))
  :config
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (setq markdown-asymmetric-header t))

(provide 'lang-markdown)
