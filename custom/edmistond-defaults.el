(load-theme 'doom-one t)

;; http://stackoverflow.com/a/8142077 for set-face-attribute example
(set-face-attribute 'default nil
		    :family "Fira Code" :height 125 :weight 'normal)

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

;; ligatures for fira code
(when (window-system)
  (set-default-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))


(provide 'edmistond-defaults)
