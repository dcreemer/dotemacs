;;; init-ido.el -- ido config

;;; Commentary:

;;; Code:

(require 'ido)

(ido-mode t)
(ido-everywhere t)

(require-package 'ido-vertical-mode)
(ido-vertical-mode 1)

(require-package 'flx-ido)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(setq ido-use-virtual-buffers t)
(setq ido-save-directory-list-file (state-file "ido.last"))

(when (maybe-require-package 'ido-ubiquitous)
  (ido-ubiquitous-mode t))

;; smarter meta-x
(require-package 'smex)
(smex-initialize)
(setq smex-save-file (state-file "smex-items"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require-package 'idomenu)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))

(provide 'init-ido)

;;; init-ido.el ends here
