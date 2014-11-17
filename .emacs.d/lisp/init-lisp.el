;;; init-lisp.el -- elisp mode

;;; Commentary:

;;; Code:

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(provide 'init-lisp)

;;; init-lisp.el ends here
