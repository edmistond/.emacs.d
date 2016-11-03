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

(defun split-window-below-and-switch ()
  "Split window horizontally and jump to new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-switch ()
  "Split window vertically and jump to new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun js-custom ()
  "js-mode-hooks"
  (tern-mode t)
  (setq js-indent-level 2))

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

;; update org-mode clock tables not to use annoying \emsp
(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "^"))
      (while (> level 2)
	(setq level (1- level)
	      str (concat str "--")))
      (concat str "-> "))))

(provide 'edmistond-functions)
