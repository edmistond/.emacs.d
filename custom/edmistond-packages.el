(require 'cl)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(package-initialize)
(setq package-archive-enable-alist '(("melpa" magit)))
(defvar edmistond/packages '(
			     ace-window
			     ag
			     avy
			     atom-one-dark-theme
			     auto-complete
			     base16-theme
			     bind-key
			     buffer-move
			     csharp-mode
			     doom-themes
			     dracula-theme
			     diminish
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
			     use-package
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

(message "init-packages complete!")
(provide 'edmistond-packages)
