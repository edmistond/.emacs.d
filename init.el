(setq user-full-name "David Edmiston"
      user-mail-address "edmistond@gmail.com")

(require 'package)
(setq backup-directory-alist `(("." . "~/.saves")))

(add-to-list 'load-path "~/.emacs.d/custom/")
(use-package edmistond-packages)
(use-package edmistond-functions)
(use-package edmistond-defaults)
(use-package edmistond-orgai)

;; configure languagetool to use a server running in docker
;; at localhost:8081, don't want to run our own server here
(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
	     languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-server-url "http://localhost/"
	languagetool-server-port "8081"))

(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

(use-package ace-window
  :bind (("M-p" . ace-window)))

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
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

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

(setq magit-last-seen-setup-instructions "1.4.0")

(global-auto-revert-mode t)

;; (use-package lsp-mode
;;   :init)
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package org-mode
  :defer t
  :init
  ;; org mode configuration
  (setq org-log-done 'time)
  (put 'erase-buffer 'disabled nil)

  ;; org-mode customizations
  (setq org-log-done t
	org-todo-keywords '((sequence "todo" "inprogress" "|" "done")))

    ;; update org-mode clock tables not to use annoying \emsp
  (advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)
  (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    '(smart-newline ruby-block robe powershell powerline-evil powerline markdown-mode magit linum-relative key-chord json-reformat inf-ruby helm-projectile helm golden-ratio evil-surround evil-leader evil enh-ruby-mode diminish dracula-theme doom-themes csharp-mode buffer-move base16-theme auto-complete atom-one-dark-theme ag ace-window use-package)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "Davids-MacBook-Pro.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" default))
 '(package-selected-packages
   '(languagetool org-ai gptel magit tree-sitter git-commit key-chord ag web-beautify use-package ujelly-theme tree-sitter-indent solarized-theme diminish atom-one-dark-theme tree-sitter-langs posframe evil-leader robe spacemacs-theme csharp-mode helm-projectile flycheck evil-surround ace-window powerline-evil undo-tree golden-ratio enh-ruby-mode json-reformat smart-newline base16-theme powershell lsp-ui dracula-theme doom-themes linum-relative buffer-move helm-lsp spaceline spacegray-theme company auto-complete web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
