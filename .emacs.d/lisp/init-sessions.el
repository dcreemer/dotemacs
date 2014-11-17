;;; init-sessions.el -- load and save sessions

;;; Commentary:

;;; Code:

;; return to same point in a buffer when revisiting the file:
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (state-file "places"))

;; where to store desktop files
(setq desktop-path (list user-state-directory)
      desktop-base-file-name "emacs.desktop"
      desktop-restore-frames t)

;; save backups to common location, and keep more versions
(setq backup-directory-alist `(("." . ,(state-file "backups"))))
(setq delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t)

(setq auto-save-list-file-prefix (state-file "auto-save-list/.saves-"))

;; save and restore history
(setq savehist-file (state-file "history"))
(setq savehist-additional-variables '(kill-ring))
(savehist-mode 1)

(desktop-save-mode 1)

(provide 'init-sessions)

;;; init-sessions.el ends here
