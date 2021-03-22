; LaTeX

(use-package tex-site
  :straight auctex
  :defer t
  :init
  (setq TeX-parse-self t
	TeX-auto-save t
  TeX-electric-math t
  LaTeX-electric-left-right-brace t
	TeX-electric-sub-and-superscript t
	TeX-quote-after-quote t
	TeX-clean-confirm nil
	TeX-source-correlate-mode t
	TeX-source-correlate-method 'synctex))

  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
	'(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (setq org-latex-pdf-process (list "latexmk -shell-escape -f -pdf -quiet -interaction=nonstopmode %f"))

;; Insert math-mode delimiters for LaTeX and ConTeXt.
(add-hook 'ConTeXt-mode-hook
	  (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
			  (cons "$" "$"))))
(add-hook 'LaTeX-mode-hook
	  (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
			  (cons "\\(" "\\)"))))

(add-hook 'LaTeX-mode-hook #'flymake-mode)

(use-package auctex-latexmk             ; latexmk command for AUCTeX
  :config (auctex-latexmk-setup))

;; Cdlatex makes inserting LaTeX easier.


(use-package cdlatex
  :init
  (setq cdlatex-math-symbol-alist
'((?. ("\\land" "\\cdot"))
  (?v ("\\lor" "\\vee"))
  (?> ("\\lif" "\\rightarrow"))
  (?= ("\\liff" "\\Leftrightarrow" "\\Longleftrightarrow"))
  (?! ("\\lneg" "\\neg"))
  (?# ("\\Box"))
  (?$ ("\\Diamond"))
   ))
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex))


;; Italics and Bold

(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (define-key LaTeX-mode-map (kbd "s-i") (kbd "\C-c \C-f \C-e"))
	     (define-key LaTeX-mode-map (kbd "s-b") (kbd "\C-c \C-f \C-b"))
	     )
	  )


;; Start Emacs server

(server-start)



;; Auto-raise Emacs on activation (from Skim, usually)

(defun raise-emacs-on-aqua()
  (shell-command "osascript -e 'tell application \"Emacs\" to activate' "))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)

;; Configure Biber
;; Allow AucTeX to use biber as well as/instead of bibtex.


;; Biber under AUCTeX
(defun TeX-run-Biber (name command file)
  "Create a process for NAME using COMMAND to format FILE with Biber." 
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function 'TeX-Biber-sentinel)
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-Biber-sentinel (process name)
  "Cleanup TeX output buffer after running Biber."
  (goto-char (point-max))
  (cond
   ;; Check whether Biber reports any warnings or errors.
   ((re-search-backward (concat
			 "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
			 "\\(warnings?\\|error messages?\\))") nil t)
    ;; Tell the user their number so that she sees whether the
    ;; situation is getting better or worse.
    (message (concat "Biber finished with %s %s. "
		     "Type `%s' to display output.")
	     (match-string 1) (match-string 2)
	     (substitute-command-keys
	      "\\\\[TeX-recenter-output-buffer]")))
   (t
    (message (concat "Biber finished successfully. "
		     "Run LaTeX again to get citations right."))))
  (setq TeX-command-next TeX-command-default))

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber"))
  )    

(defun tex-clean ()
  (interactive)
  (shell-command "latexmk -c"))

(defun tex-clean-all ()
  (interactive)
  (shell-command "latexmk -C"))


;; Beamer

(setq LaTeX-paragraph-commands '("pause" "blpause"))


;; Dim tilde

(add-hook
 'TeX-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("~" . 'font-latex-sedate-face)))))

;; Start latexmk continuous preview.

(defun rlr/tex-pvc ()
  "Compile continuously with latexmk."
  (interactive)
  (async-shell-command (concat "mkpvc " (buffer-file-name)))
  (delete-other-windows)
  (TeX-view))


;; Move to edited position after save.

(add-hook 'after-save-hook
	  (lambda ()
	    (when (string= major-mode 'latex-mode)
	      (TeX-view))))

;; Revert PDF after compilation has finished
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; Modify Bibtex completion in Org mode.


(setq bibtex-completion-cite-default-command "autocite")
(defun bibtex-completion-format-citation-orgref (keys)
  "Formatter for Org mode citation commands.
Prompts for the command and for arguments if the commands can
take any.  If point is inside or just after a citation command,
only adds KEYS to it."
  (let (macro)
    (cond
     ((and (require 'reftex-parse nil t)
           (setq macro (reftex-what-macro 1))
           (stringp (car macro))
           (string-match "\\`\\\\cite\\|cite\\'" (car macro)))
      ;; We are inside a cite macro.  Insert key at point, with appropriate delimiters.
      (delete-horizontal-space)
      (concat (pcase (preceding-char)
                (?\{ "")
                (?, " ")
                (_ ", "))
              (s-join ", " keys)
              (if (member (following-char) '(?\} ?,))
		  ""
                ", ")))
     ((and (equal (preceding-char) ?\})
           (require 'reftex-parse nil t)
           (save-excursion
             (forward-char -1)
             (setq macro (reftex-what-macro 1)))
           (stringp (car macro))
           (string-match "\\`\\\\cite\\|cite\\'" (car macro)))
      ;; We are right after a cite macro.  Append key and leave point at the end.
      (delete-char -1)
      (delete-horizontal-space t)
      (concat (pcase (preceding-char)
                (?\{ "")
                (?, " ")
                (_ ", "))
              (s-join ", " keys)
              "}"))
     (t
      ;; We are not inside or right after a cite macro.  Insert a full citation.
      (let* ((initial (when bibtex-completion-cite-default-as-initial-input
                        bibtex-completion-cite-default-command))
             (default (unless bibtex-completion-cite-default-as-initial-input
                        bibtex-completion-cite-default-command))
             (default-info (if default (format " (default \"%s\")" default) ""))
             (cite-command (completing-read
                            (format "Cite command%s: " default-info)
                            bibtex-completion-cite-commands nil nil initial
                            'bibtex-completion-cite-command-history default nil)))
        (if (member cite-command '("nocite" "supercite"))  ; These don't want arguments.
            (format "\\%s{%s}" cite-command (s-join ", " keys))
          (let ((prenote (if bibtex-completion-cite-prompt-for-optional-arguments
                             (read-from-minibuffer "Prenote: ")
                           ""))
                (postnote (if bibtex-completion-cite-prompt-for-optional-arguments
                              (read-from-minibuffer "Postnote: ")
                            "")))
            (cond ((and (string= "" prenote) (string= "" postnote))
                   (format "[[%s:%s]]" cite-command (s-join ", " keys)))
                  (t
                   (format "[[%s:%s][%s::%s]]" cite-command (s-join ", " keys) prenote postnote)
                   )))))))))


;; Use Ivy-bibtex for reference completions.
;; (use-package ivy-bibtex
;;   ;; :bind ("s-4" . ivy-bibtex)
;;   :after (ivy)
;;   :config
;;   (setq bibtex-completion-bibliography '("/Users/rlridenour/Dropbox/bibtex/rlr.bib"))
;;   (setq reftex-default-bibliography '("/Users/rlridenour/Dropbox/bibtex/rlr.bib"))
;;   (setq bibtex-completion-pdf-field "File")
;;   (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
;;   (setq bibtex-completion-format-citation-functions
;; 	'((org-mode      . bibtex-completion-format-citation-orgref)
;; 	  (latex-mode    . bibtex-completion-format-citation-cite)
;; 	  ;; (markdown-mode    . bibtex-completion-format-citation-cite)
;; 	  (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
;; 	  (default       . bibtex-completion-format-citation-default))))

(use-package bibtex-actions
  :straight (:host github :repo "bdarcus/bibtex-actions")
  :config
  (setq bibtex-completion-bibliography "~/Dropbox/bibtex/rlr.bib")
  (setq bibtex-completion-format-citation-functions
	'((org-mode      . bibtex-completion-format-citation-orgref)
	  (latex-mode    . bibtex-completion-format-citation-cite)
	  ;; (markdown-mode    . bibtex-completion-format-citation-cite)
	  (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
	  (default       . bibtex-completion-format-citation-default))))

(use-package ebib
  :defer t
  :config
  (setq ebib-bibtex-dialect 'biblatex)
  :custom
  (ebib-preload-bib-files '("~/Dropbox/bibtex/rlr.bib")))

	  

(provide 'lang-latex)
