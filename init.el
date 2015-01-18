(setq user-full-name "David Edmiston")
(setq user-mail-address "edmistond@gmail.com")

(require 'package)
(require 'cl)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(package-initialize)
(setq package-archive-enable-alist '(("melpa" magit)))

(defvar edmistond/packages '(ace-jump-mode
			     auto-complete
			     base16-theme
			     buffer-move
			     enh-ruby-mode
			     evil
			     evil-leader
			     evil-surround
			     git-commit-mode
			     git-rebase-mode
			     goto-chg
			     inf-ruby
			     json-reformat
			     key-chord
			     linum-relative
			     magit
			     markdown-mode
			     popup
			     powerline
			     powerline-evil
			     robe
			     ruby-block
			     ruby-electric
			     ruby-end
			     ruby-guard
			     ruby-hash-syntax
			     ruby-interpolation
			     rvm
			     smart-newline
			     solarized-theme
			     spacegray-theme
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

;; (defun edmistond/eval-and-clear ()
;;   "Evaluates a region and then unselects it."
;;   (interactive)
;;   (eval-region)
;;   (set 'deactivate-mark t))

;; don't want the scroll or tool bars
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-default 'truncate-lines t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(delete-selection-mode 1)

(load-theme 'solarized-light t)
;; (set-default-font "SourceCodePro-Medium-12")
(set-face-attribute 'default nil
		    :family "Source Code Pro" :height 122 :weight 'normal)

(setq tab-width 2
      indent-tabs-mode nil)

(setq make-backup-files nil)

;; disable command-as-super, because i keep hitting cmd-x to kill region
;; by accident and it's super (hah) annoying
(setq ns-command-modifier nil)

;; Do you really want to type yes and no ALL THE TIME?
(defalias 'yes-or-no-p 'y-or-n-p)

;; highlight matching brackets when putting the point on them. 
(show-paren-mode 1)
(electric-pair-mode t)

(put 'erase-buffer 'disabled nil)

;; **** evil mode ****

;; set up evil-leader
(require 'evil-leader)
(global-evil-leader-mode 1)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'ibuffer
  "k" 'kill-buffer
  "x" 'execute-extended-command
  "j" 'ace-jump-mode
  "er" 'eval-region
  "eb" 'erase-buffer
  ;; "ev" 'edmistond/eval-and-clear
  "ms" 'magit-status
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

(setq evil-move-cursor-back nil)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-motion-state-map (kbd ";") 'evil-ex)
;; **** end evil mode ****

;; use meta-[arrow keys] to move the point between different
;; emacs windows in a frame. this keeps shift-[arrows] free for
;; more useful things like marking regions.
(windmove-default-keybindings 'meta)

;; prevent markdown-mode from stealing the windmove meta keybindings
(add-hook 'markdown-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<M-right>") 'windmove-right)
	     (local-set-key (kbd "<M-left>") 'windmove-left)
	     (local-set-key (kbd "<M-up>") 'windmove-up)
	     (local-set-key (kbd "<M-down>") 'windmove-down)))


;; ;; auto-indent various kinds of files
;; (add-hook 'js-mode-hook '(lambda ()
;; 			   (local-set-key (kbd "RET") 'newline-and-indent)))

;; (add-hook 'js-mode-hook '(lambda ()
;; 			   (setq js-indent-level 2)))

;; (add-hook 'ruby-mode-hook '(lambda ()
;; 			     (local-set-key (kbd "RET") 'newline-and-indent)))

;; (add-hook 'enh-ruby-mode-hook '(lambda ()
;; 				 (local-set-key (kbd "RET") 'newline-and-indent)))

;; (add-hook 'web-mode-hook '(lambda ()
;; 			    (local-set-key (kbd "RET") 'newline-and-indent)))


;; web-mode.el settings, mostly to have certain kinds of files automatically trigger it.
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "RET") 'smart-newline)

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
(global-set-key (kbd "C-c e b") 'erase-buffer)

;; org mode configuration
(setq org-log-done 'time)
(put 'erase-buffer 'disabled nil)

;; **** ruby and robe mode configuration ****
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . robe-mode))

;; TODO: additional Ruby configuration

;; TODO: javascript/js2 mode configuration


(require 'linum-relative)
(linum-mode 1)

