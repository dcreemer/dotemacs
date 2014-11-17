;;; init-python.el -- setup python dev environment

;;; Commentary:

;;; Code:

(require-package 'python-mode)
(require-package 'pip-requirements)
(require-package 'anaconda-mode)
(require-package 'company-anaconda)

(add-auto-mode 'python-mode "\\.py$")

(require 'python-mode)
(setq py-electric-colon-active t)

(require 'company)
(add-to-list 'company-backends 'company-anaconda)

(add-hook 'python-mode-hook
          (lambda ()
            (anaconda-mode 1)
            (eldoc-mode 1)
            (setq flycheck-flake8-maximum-line-length 100)))

(require 'anaconda-mode)
(define-key anaconda-mode-map (kbd "M-,") 'anaconda-nav-pop-marker)

(provide 'init-python)

;;; init-python.el ends here
