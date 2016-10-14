(setq user-full-name "David Edmiston")
(setq user-mail-address "edmistond@gmail.com")

(require 'package)
(require 'cl)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(package-initialize)
(setq package-archive-enable-alist '(("melpa" magit)))
(defvar edmistond/packages '(
			     ace-window
			     ag
			     avy
			     auto-complete
			     base16-theme
			     buffer-move
			     csharp-mode
			     dracula-theme
			     enh-ruby-mode
			     evil
			     evil-leader
			     evil-surround
			     golden-ratio
			     goto-chg
			     helm
			     helm-projectile
			     inf-ruby
			     json-reformat
			     key-chord
			     linum-relative
			     magit
			     markdown-mode
			     popup
			     powerline
			     powerline-evil
			     powershell
			     projectile
			     robe
			     ruby-block
			     ruby-electric
			     ruby-end
			     ruby-guard
			     ruby-hash-syntax
			     ruby-interpolation
			     rvm
			     smart-newline
			     spaceline
			     solarized-theme
			     spacegray-theme
			     spacemacs-theme
			     ujelly-theme
			     undo-tree
			     web-beautify
			     web-mode)
  "Default Packages")

(defun edmistond/packages-installed-p ()
  (loop for pkg in edmistond/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (edmistond/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg edmistond/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(add-to-list 'load-path "~/.emacs.d/custom/")
(autoload 'jscs-indent-apply "jscs" nil t)
(autoload 'jscs-fix "jscs" nil t)
(autoload 'jscs-fix-before-save "jscs" nil t)


;; don't want the scroll or tool bars
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; helm and projectile setup
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-indexing-method 'alien)

;; start flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)

(set-default 'truncate-lines t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(delete-selection-mode 1)

(load-theme 'base16-eighties-dark t)

;; http://stackoverflow.com/a/8142077 for set-face-attribute example
(set-face-attribute 'default nil
		    :family "Menlo" :height 110 :weight 'normal)

(global-auto-revert-mode t)

(setq tab-width 2
      indent-tabs-mode nil)

(setq make-backup-files nil)
(setq auto-save-default nil)

;; disable command-as-super, because i keep hitting cmd-x to kill region
;; by accident and it's super (hah) annoying
;;(setq ns-command-modifier nil)

;; Do you really want to type yes and no ALL THE TIME?
(defalias 'yes-or-no-p 'y-or-n-p)

;; highlight matching brackets when putting the point on them. 
(show-paren-mode 1)
(electric-pair-mode t)

(put 'erase-buffer 'disabled nil)

;; **** global emacs key redefinitions ****
(global-set-key (kbd "C-c e b") 'erase-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer) 
(global-set-key (kbd "RET") 'smart-newline)

(global-set-key (kbd "C-c C-s C-w") 'delete-trailing-whitespace)

;; *******************
;; **** evil mode ****
;; *******************

;; set up evil-leader
(require 'evil-leader)
(global-evil-leader-mode 1)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'ibuffer
  "k" 'kill-buffer
  "x" 'execute-extended-command
  "j" 'avy-goto-word-or-subword-1
  "df" 'delete-frame
  "mf" 'make-frame
  "er" 'eval-region
  "eb" 'erase-buffer
  "ms" 'magit-status
  "tfd" 'tern-find-definition
  "cc" 'comment-region
  "uc" 'uncomment-region)

;; set up evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'powerline)
(powerline-evil-vim-color-theme)
(display-time-mode t)

;; actually enable evil mode
(require 'evil)
(evil-mode 1)

;; use key-chord to remap insert mode escape to 'jj'
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

;; don't move the cursor back a line when leaving insert
(setq evil-move-cursor-back nil)

;; remap ; to : in normal mode for quicker access to evil command line
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-motion-state-map (kbd ";") 'evil-ex)

;; http://stackoverflow.com/a/20899418
;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
; Make horizontal movement cross lines                                    
(setq-default evil-cross-lines t)

(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-a" 'beginning-of-line)

;; **** end evil mode ****
;; ***********************

;; use meta-[arrow keys] to move the point between different
;; emacs windows in a frame. this keeps shift-[arrows] free for
;; more useful things like marking regions. although with evil
;; visual mode, may not matter!
(windmove-default-keybindings 'meta)

(global-set-key (kbd "M-p") 'ace-window)

;; prevent markdown-mode from stealing the windmove meta keybindings
(add-hook 'markdown-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<M-right>") 'windmove-right)
	     (local-set-key (kbd "<M-left>") 'windmove-left)
	     (local-set-key (kbd "<M-up>") 'windmove-up)
	     (local-set-key (kbd "<M-down>") 'windmove-down)))

;; web-mode.el settings, mostly to have certain kinds of files automatically trigger it.
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; autocomplete config
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-auto-show-menu 0.2
      ac-delay 0.2
      ac-menu-height 20
      ac-auto-start t
      ac-show-menu-immediately-on-auto-complete t
      ac-use-fuzzy 1)
(add-to-list 'ac-modes 'enh-ruby-mode)

;; rvm.el config
(global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)

;; inf-ruby config
(global-set-key (kbd "C-c r r") 'inf-ruby)

;; org mode configuration
(setq org-log-done 'time)
(put 'erase-buffer 'disabled nil)


;; **** ruby and robe mode configuration ****
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; TODO: additional Ruby configuration

;;TODO: javascript/js2 mode configuration
(defun js-custom ()
  "js-mode-hooks"
  (tern-mode t)
  (setq js-indent-level 2))

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))


(add-hook 'js-mode-hook 'js-custom)
;; (eval-after-load 'js-mode
;;   '(define-key js-mode-map "M-." 'tern-find-definition))

(require 'linum)
(require 'linum-relative)
;; (global-linum-mode t)

(setq magit-last-seen-setup-instructions "1.4.0")

(global-auto-revert-mode t)

;; (setq-default flycheck-disabled-checkers '(javascript-jshint))
;; (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
;; (setq-default flycheck-disabled-checkers '(emacs-lisp))

(setq-default flycheck-disabled-checkers
              (append '(javascript-jshint
                        javascript-jscs
                        emacs-lisp
                        emacs-lisp-checkdoc)))

;; org-mode customizations
(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "CANNOTREPRO" "NEEDINFO" "DEFERRED" "|" "DONE" "DEPLOYED" "WONTFIX"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "SteelBlue1" :weight bold))
			       ("DEPLOYED" . (:foreground "pale green" :weight bold))
			       ("WONTFIX" . (:foreground "indian red" :weight bold))
			       ("CANNOTREPRO" . (:foreground "red" :weight bold))
			       ("NEEDINFO" . (:foreground "yellow1" :weight bold))
			       ("DEFERRED" . (:foregorund "cornsilk3" :weight bold))))

;; update org-mode clock tables not to use annoying \emsp
(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "^"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str "-> "))))

(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)
(setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

(add-hook 'jade-mode-hook
	  '(lambda ()
	     (setq tab-width 2)))

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))
