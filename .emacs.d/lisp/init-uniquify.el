;;; init-uniquify.el -- customize uniquify

;;; Commentary:

;;; Code:

(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'init-uniquify)

;;; init-uniquify.el ends here
