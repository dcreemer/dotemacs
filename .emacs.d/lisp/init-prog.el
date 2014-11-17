;;; init-prog.el -- generic programming mode settings

;;; Commentary:
;;

;;; Code:

;; highlight-symbols in all programming modes
(require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook yaml-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(after-load 'highlight-symbol
  (diminish 'highlight-symbol-mode))

;; rainbow parens
(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; show indents
(require-package 'indent-guide)
(add-hook 'prog-mode-hook 'indent-guide-mode)
(after-load 'indent-guide
  (diminish 'indent-guide-mode))

;; yasnippet
(require-package 'yasnippet)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(after-load 'yasnippet
  (diminish 'yas-minor-mode))

(provide 'init-prog)

;;; init-prog.el ends here
