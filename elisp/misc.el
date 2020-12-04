(use-package diminish)

(eval-after-load "flyspell"
  '(diminish 'flyspell-mode))
(diminish 'visual-line-mode)
(diminish 'abbrev-mode)
(diminish 'auto-revert-mode)
(eval-after-load "reftex"
  '(diminish 'reftex-mode))

(setq default-directory "~/")


(provide 'misc)
