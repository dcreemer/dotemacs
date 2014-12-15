;;; init-lisp.el -- elisp mode

;;; Commentary:

;;; Code:

(require-package 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(provide 'init-lisp)

;;; init-lisp.el ends here
