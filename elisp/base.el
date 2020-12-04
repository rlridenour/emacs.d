(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode
      make-pointer-invisible t
      display-time-24hr-format t
      display-time-day-and-date t)

;; Disable bell and flash modeline
  (defun my-terminal-visible-bell ()
	"A friendlier visual bell effect."
	(invert-face 'mode-line)
	(run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function 'my-terminal-visible-bell)

;; Turn on syntax highlighting for all buffers
(global-font-lock-mode t)

;; Match parentheses
(show-paren-mode 1)

;; Turn off tool bar and scroll bar
(scroll-bar-mode -1)
(tool-bar-mode -1)


;; Show line numbers and column numbers in mode line
(line-number-mode 1)
(column-number-mode 1)

;; Turn off winner mode
(winner-mode 0)

;; Display line numbers
(global-display-line-numbers-mode)

;; Set the modifier keys in OS X
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq ns-function-modifier 'hyper)


;; Always prefer UTF-8 encoding.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


;; Use TeX to input special characters. Activated later for text, markdown, and org modes.
(setq default-input-method 'TeX)

;; Auto save often — save every 20 characters typed (this is the minimum)
(setq auto-save-interval 20)


;; Use "y" and "n":
(defalias 'yes-or-no-p 'y-or-n-p)


;; Confirm killing emacs on graphical sessions:
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))


;; Edit by Visual Lines
(global-visual-line-mode t)


;; Navigate visual lines:
(setq line-move-visual t)


;; Single space ends sentence:
(setq sentence-end-double-space nil)

;; Save backups and auto-saves to the system temp directory.


(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))
  (setq backup-by-copying t)
  (setq delete-old-versions t
		kept-new-versions 6
		kept-old-versions 2
		version-control t)

;; Recent files
(require 'recentf)
(setq recentf-max-saved-items 200
  recentf-max-menu-items 15)
(recentf-mode)

;; Use spotlight for locate.

(setq locate-command "mdfind")


;; Open links in default Mac browser.

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Don't ask for confirmation to kill processes when exiting Emacs. Credit to [[http://timothypratley.blogspot.com/2015/07/seven-specialty-emacs-settings-with-big.html][Timothy Pratley]].


(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (cl-flet ((process-list ())) ad-do-it))


;; ibuffer

;; Don't ask for unnecessary confirmations


(setq ibuffer-expert t)


;; Auto-update buffer list


(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups "home")))

;; Shell

;; This kills the buffer after closing the terminal.

(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
	 (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
	(if (string= event "finished\n")
	    (kill-buffer ,buff))))))
(add-hook 'term-exec-hook 'oleh-term-exec-hook)


;; To paste into term.


(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))


;; Make completion case-insensitive in eshell


(setq eshell-cmpl-ignore-case t)
(setq pcomplete-ignore-case t)


;; Start eshell


(global-set-key (kbd "C-x m") (lambda () (interactive) (eshell t)))
;; Start a new eshell even if one is active
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))


;; Start a regular shell


(global-set-key (kbd "C-x M-m") 'shell)



;; Misc

;; Kill contents of scratch buffer, not the buffer itself. From [[http://emacswiki.org/emacs/RecreateScratchBuffer][TN]].


(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn
	(delete-region (point-min) (point-max))
	nil)
    t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)


;; Mark date and time that files were saved.


(add-hook 'before-save-hook 'time-stamp)


;; Move deleted files to system trash.


(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Use GNU ls to avoid "Listing directory failed but 'access-file' worked" error.


(setq insert-directory-program "/usr/local/bin/gls"); use proper GNU ls




(provide 'base)
;;; base ends here
