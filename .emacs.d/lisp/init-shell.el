;;; init-shell.el -- setup various interactive shells

;;; Commentary:
;;  currently favoring eshell

;;; Code:

(require 'eshell)
(global-set-key (kbd "C-$") 'eshell)
(setq eshell-directory-name (state-file "eshell")
      eshell-aliases-file (expand-file-name "eshell-aliases" user-emacs-directory)
      ;; default shell prompt has an extra space I don't like.
      ;; remove it:
      eshell-prompt-function (function
                              (lambda ()
                                (concat (abbreviate-file-name (eshell/pwd))
                                        (if (= (user-uid) 0) "# " "$ "))))
      eshell-prompt-regexp "^[^#$\n]*[#$] ")

(require 'em-term)
(add-to-list 'eshell-visual-commands "ssh")

;; 'clear' command for eshell
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; git completion for eshell
(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "available git commands in")
    (let (commands)
      (while (re-search-forward
	      "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
	      nil t)
	(push (match-string 1) commands)
	(when (match-string 2)
	  (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
	(push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
			     (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

;; tramp
(setq tramp-default-method "ssh")
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(provide 'init-shell)

;;; init-shell.el ends here