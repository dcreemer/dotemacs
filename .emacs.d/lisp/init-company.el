;;; init-company.el -- settings for company auto-complete mode

;;; Commentary:

;;; Code:

(require-package 'company)

(add-hook 'prog-mode-hook
          (lambda ()
            (company-mode 1)
            (diminish 'company-mode)
            (define-key (current-local-map) (kbd "M- ") 'company-complete)))

(provide 'init-company)

;;; init-company.el ends here
