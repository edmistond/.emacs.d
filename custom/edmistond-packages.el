(require 'cl)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(package-initialize)
(setq package-archive-enable-alist '(("melpa" magit company)))
(defvar edmistond/packages '(
			     ace-window
			     ag
			     alchemist
			     avy
			     atom-one-dark-theme
			     auto-complete
			     base16-theme
			     bind-key
			     buffer-move
			     csharp-mode
			     company
			     company-tern
			     doom-themes
			     dracula-theme
			     diminish
			     enh-ruby-mode
			     elixir-mode
			     evil
			     evil-leader
			     evil-surround
			     exec-path-from-shell
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
			     mmm-mode
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
			     tern
			     tide
			     rvm
			     smart-newline
			     smartparens
			     spaceline
			     solarized-theme
			     spacegray-theme
			     spacemacs-theme
			     stylus-mode
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
