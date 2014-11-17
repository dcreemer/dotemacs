;;; init-recentf.el -- remember many recent files

;;; Commentary:

;;; Code:

(require 'recentf)

(setq recentf-max-saved-items 1000
      recentf-max-menu-items 15
      recentf-save-file (state-file "recentf")
      recentf-exclude '("/tmp/" "/ssh:"))

(recentf-mode +1)

(global-set-key (kbd "C-c f") 'recentf-ido-find-file)

(provide 'init-recentf)

;;; init-recentf.el ends here
