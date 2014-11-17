;;; init-windows.el -- customize window handling

;;; Commentary:

;;; Code:

;; Use shift keys ...
(windmove-default-keybindings 'shift)

;; and M-P (via ace-window) to navigate windows
(require-package 'ace-window)
(global-set-key (kbd "M-p") 'ace-window)

(provide 'init-windows)

;;; init-windows.el ends here
