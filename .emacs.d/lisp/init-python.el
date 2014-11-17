;;; init-python.el -- setup python dev environment
;;

;;; Commentary:

;;; Code:

(require-package 'python-mode)
(require-package 'pip-requirements)
(require-package 'py-autopep8)
(require-package 'anaconda-mode)
(require-package 'company-anaconda)

(add-auto-mode 'python-mode "\\.py$")
(setq py-electric-colon-active t
      py-autopep8-options '("--max-line-length=100"))

(require 'company)
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(define-key anaconda-mode-map (kbd "M-,") 'anaconda-nav-pop-marker)

(provide 'init-python)

;;; init-python.el ends here
