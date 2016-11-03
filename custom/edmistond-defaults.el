(load-theme 'atom-one-dark t)

;; http://stackoverflow.com/a/8142077 for set-face-attribute example
(set-face-attribute 'default nil
		    :family "Menlo" :height 110 :weight 'normal)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)
(delete-selection-mode 1)
(global-auto-revert-mode t)

(setq inhibit-startup-message t
      initial-scratch-message nil
      tab-width 2
      indent-tabs-mode nil
      make-backup-files nil
      auto-save-default nil)

;; disable command-as-super, because i keep hitting cmd-x to kill region
;; by accident and it's super (hah) annoying
;;(setq ns-command-modifier nil)

;; Do you really want to type yes and no ALL THE TIME?
(defalias 'yes-or-no-p 'y-or-n-p)

;; highlight matching brackets when putting the point on them. 
(show-paren-mode 1)
(electric-pair-mode t)

(put 'erase-buffer 'disabled nil)

;; use meta-[arrow keys] to move the point between different
;; emacs windows in a frame. this keeps shift-[arrows] free for
;; more useful things like marking regions. although with evil
;; visual mode, may not matter!
(windmove-default-keybindings 'meta)

(bind-keys ("C-c e b" . erase-buffer)
	   ("C-x C-b" . ibuffer)
	   ("RET" . smart-newline)
	   ("C-x 2" . split-window-below-and-switch)
	   ("C-x 3" . split-window-right-and-switch))

(provide 'edmistond-defaults)
