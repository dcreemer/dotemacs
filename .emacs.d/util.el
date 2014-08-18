;;
;; various found and somewhat helpful functions
;;

;; format and XML region / buffer
;; found on http://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs
;; 
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    ;; split <foo><bar> or </foo><bar>, but not <foo></foo>
    (goto-char begin)
    (while (search-forward-regexp ">[ \t]*<[^/]" end t)
      (backward-char 2) (insert "\n") (incf end))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
      (backward-char) (insert "\n") (incf end))
    ;; put xml namespace decls on newline
    (goto-char begin)
    (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
      (goto-char (match-end 0))
      (backward-char 6) (insert "\n") (incf end))
    (indent-region begin end nil)
    (normal-mode))
  (message "All indented!"))


;; use IDO to find recent files
;; from http://emacsredux.com/blog/2013/04/05/recently-visited-files/
;;
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;;
;; insert current UTC timestamp
;;
(defun insert-utc ()
  "Insert string for the current UTC time formatted like '2014-02-08T01:30:15Z'"
  (interactive)
  (let ((now (current-time)))
    (set-time-zone-rule t)
    (insert (format-time-string "%FT%TZ" now))
    (set-time-zone-rule nil)))

(defun google ()
  "Googles a query or region if any."
  (interactive)
  (w3m-browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if (region-active-p)
        (buffer-substring (region-beginning) (region-end))
      (read-string "Query: ")))))

;; 'clear' command for eshell
;; from http://lists.gnu.org/archive/html/help-gnu-emacs/2007-08/msg00836.html
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; I keep my transient state in a custom directory
(defvar user-state-directory
  (expand-file-name "state" user-emacs-directory)
  "default directory for transient user state")

(defun state-file (path)
  (expand-file-name path user-state-directory))

;;
;; load and save elscreen configuration
;;
(defvar elscreen-tab-configuration-store-filename
    (concat user-emacs-directory "state/elscreen"))

(defun elscreen-store-hook ()
  "Store the elscreen tab configuration."
  (with-temp-file elscreen-tab-configuration-store-filename
    (insert (prin1-to-string (elscreen-get-screen-to-name-alist)))))

(defun elscreen-load-hook ()
  (let ((screens (reverse
                  (read
                   (with-temp-buffer
                     (insert-file-contents elscreen-tab-configuration-store-filename)
                     (buffer-string))))))
    (while screens
      (setq screen (car (car screens)))
      (setq buffers (split-string (cdr (car screens)) ":"))
      (if (eq screen 0)
          (switch-to-buffer (car buffers))
        (elscreen-find-and-goto-by-buffer (car buffers) t t))
      (while (cdr buffers)
        (switch-to-buffer-other-window (car (cdr buffers)))
        (setq buffers (cdr buffers)))
      (setq screens (cdr screens)))
    (elscreen-goto 0)))

;; eshell pcomplete commands:

;; git completion
;; from https://tsdh.wordpress.com/2013/05/31/eshell-completion-for-git-bzr-and-hg/

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
