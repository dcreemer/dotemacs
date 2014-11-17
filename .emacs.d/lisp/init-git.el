;;; init-git.el -- git VC specific settings

;;; Commentary:

;;; Code:

(require-package 'magit)
(require-package 'git-gutter)
(require-package 'git-messenger)
(require-package 'git-timemachine)
(require-package 'git-blame)
(require-package 'git-commit-mode)

(setq vc-follow-symlinks t)

(setq-default magit-process-popup-time 10
              magit-diff-refine-hunk t
              magit-completing-read-function 'magit-ido-completing-read)

(diminish 'magit-auto-revert-mode)

(global-git-gutter-mode +1)
(diminish 'git-gutter-mode)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(provide 'init-git)

;;; init-git.el ends here
