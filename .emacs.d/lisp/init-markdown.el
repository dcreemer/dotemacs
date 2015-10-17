;;; init-markdown.el -- customize markdown

;;; Commentary:

;;; Code:

(require-package 'markdown-mode)

(add-auto-mode 'markdown-mode "\\.md$" "\\.markdown$")

(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'fci-mode)

(provide 'init-markdown)

;;; init-markdown.el ends here
