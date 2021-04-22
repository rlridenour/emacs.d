

;; brew install coreutils
;; (if (executable-find "gls")
;;     (progn
;;       (setq insert-directory-program "gls")
;;       (setq dired-listing-switches "-lFGh1v --group-directories-first"))
;;   (setq dired-listing-switches "-hlF"))

(setq insert-directory-program "/usr/local/bin/gls"); use proper GNU ls


; dired-x: to hide uninteresting files in dired
(use-package dired-x
  :straight nil
  :bind ("C-x C-j" . dired-jump)
  :hook ((dired-mode . dired-omit-mode))
  :config
  (setq dired-omit-verbose nil)
  ;; hide backup, autosave, *.*~ files
  ;; omit mode can be toggled using `C-x M-o' in dired buffer.
  (setq-default dired-omit-extensions '("fdb_latexmk" "aux" "bbl" "blg" "fls" "glo" "idx" "ilg" "ind" "ist" "log" "out" "gz" "bcf" "run.xml"  "DS_Store" "auctex-auto"))

  (setq dired-omit-files
        (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|^.git$\\|^\\..+$")))

(use-package dired+)

(define-key dired-mode-map [?\M-\r] 'crux-open-with)

(provide 'base-dired)
