;;; init-org.el -- setup org

;;; Commentary:

;;; Code:

;; everyone loves org-mode
(require-package 'org)

(setq dc/notes-dir "~/Dropbox/Documents/Notes")

(setq org-startup-indented t
      org-startup-folded "content"
      org-agenda-files (list dc/notes-dir)
      org-default-notes-file (concat dc/notes-dir "/inbox.org")
      org-refile-targets '((org-agenda-files . (:tag . "refile")))
      org-outline-path-complete-in-steps t
      org-refile-use-outline-path t)

;; epresent for presentations
(require-package 'epresent)

(global-set-key (kbd "\C-c a") 'org-agenda)
(global-set-key (kbd "\C-c l") 'org-store-link)
(global-set-key (kbd "\C-c c") 'org-capture)

;; deft for managing org notes
(require-package 'deft)

(setq deft-directory dc/notes-dir
      deft-extensions '("org" "txt" "text" "md" "markdown")
      deft-use-filename-as-title t
      deft-default-extension "org")

;; (setq deft-use-filename-as-title t)
;; (setq deft-use-filter-string-for-filename t)
(setq deft-auto-save-interval 5.0)

;;key to launch deft
(global-set-key (kbd "C-c d") 'deft)

(provide 'init-org)

;;; init-org.el ends here
