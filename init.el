(setq user-full-name "David Edmiston"
      user-mail-address "edmistond@gmail.com")

(require 'package)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/custom/")
(use-package edmistond-packages)
(use-package edmistond-functions)
(use-package edmistond-defaults)

(use-package smartparens)

(use-package ace-window
  :bind (("M-p" . ace-window)))

;; (use-package jscs
;;   :init
;;   (autoload 'jscs-indent-apply "jscs" nil t)
;;   (autoload 'jscs-fix "jscs" nil t)
;;   (autoload 'jscs-fix-before-save "jscs" nil t))

(use-package helm
  :defer t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
	 ("C-h a" . helm-apropos))
  :config
  (helm-mode 1))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'helm
	projectile-indexing-method 'alien
	projectile-enable-caching t)
  (projectile-global-mode)
  (helm-projectile-on))

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode)

(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :init
  ;; start flycheck globally
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
		(append '(javascript-jshint
			  javascript-jscs
			  emacs-lisp
			  emacs-lisp-checkdoc))))

(use-package evil
  :defer t
  :init

  (use-package evil-surround
    :defer t
    :init
    (require 'evil-surround)
    (global-evil-surround-mode 1))

  (use-package evil-leader
    :defer t
    :init
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
      "uc" 'uncomment-region))

  (use-package powerline
    :defer t
    :init
    (powerline-evil-vim-color-theme)
    (display-time-mode t))

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
  (define-key evil-insert-state-map "\C-a" 'beginning-of-line))

;; prevent markdown-mode from stealing the windmove meta keybindings
(use-package markdown-mode
  :defer t
  :init
  (add-hook 'markdown-mode-hook
	    '(lambda ()
	       (local-set-key (kbd "<M-right>") 'windmove-right)
	       (local-set-key (kbd "<M-left>") 'windmove-left)
	       (local-set-key (kbd "<M-up>") 'windmove-up)
	       (local-set-key (kbd "<M-down>") 'windmove-down))))

;; web-mode.el settings, mostly to have certain kinds of files automatically trigger it.
(use-package web-mode
  :defer t
  :init 
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

;; autocomplete config
(use-package company
  :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode))
;; (require 'auto-complete)
;; (global-auto-complete-mode t)
;; (setq ac-auto-show-menu 0.2
;;       ac-delay 0.2
;;       ac-menu-height 20
;;       ac-auto-start t
;;       ac-show-menu-immediately-on-auto-complete t
;;       ac-use-fuzzy 1)
;; (add-to-list 'ac-modes 'enh-ruby-mode)

(use-package elixir-mode
  :defer t
  :init
  (add-hook 'elixir-mode-hook
	    (defun auto-activate-ruby-end-mode-for-elixir-mode ()
	      (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
		   "\\(?:^\\|\\s-+\\)\\(?:do\\)")
	      (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
	      (ruby-end-mode +1)))
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "fn" "end"
		   :when '(("SPC" "RET"))
		   :actions '(insert navigate))
    (sp-local-pair "do" "end"
		   :when '(("SPC" "RET"))
		   :post-handlers '(sp-ruby-def-post-handler)
		   :actions '(insert navigate)))
  (use-package alchemist))

;; rvm.el config
(global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)

;; inf-ruby config
(global-set-key (kbd "C-c r r") 'inf-ruby)


;; **** ruby and robe mode configuration ****
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; TODO: additional Ruby configuration

;;TODO: javascript/js2 mode configuration

(use-package npm-mode
  :init
  (npm-global-mode))

(use-package tern
  :defer t
  :init
  (use-package company-tern
    :init
    (add-to-list 'company-backends 'company-tern))
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup)))

  (add-hook 'js-mode-hook 'js-custom))
;; (eval-after-load 'js-mode
;;   '(define-key js-mode-map "M-." 'tern-find-definition))

(setq magit-last-seen-setup-instructions "1.4.0")

(global-auto-revert-mode t)
(global-hl-line-mode)

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide)

(setq company-tooltip-align-annotations t)
(setq tide-format-options '(:tabSize 2 :indentSize 2))

(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)

;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
;; (mmm-add-mode-ext-class 'markdown-mode "\\.md\\'" 'js-mode)



(use-package org-mode
  :defer t
  :init
  ;; org mode configuration
  (setq org-log-done 'time)
  (put 'erase-buffer 'disabled nil)

  (setq org-src-fontify-natively t)
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
  (advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)
  (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))))

(add-hook 'jade-mode-hook
	  '(lambda ()
	     (setq tab-width 2)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dockerfile-mode npm-mode hexo mmm-mode stylus-mode tide exec-path-from-shell tern-auto-complete company-tern tern smartparens alchemist web-mode web-beautify use-package ujelly-theme spacemacs-theme spaceline spacegray-theme solarized-theme smart-newline rvm ruby-interpolation ruby-hash-syntax ruby-guard ruby-end ruby-electric ruby-block robe powershell powerline-evil markdown-mode magit linum-relative key-chord json-reformat helm-projectile golden-ratio evil-surround evil-leader enh-ruby-mode dracula-theme doom-themes csharp-mode company buffer-move base16-theme auto-complete atom-one-dark-theme ag ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
