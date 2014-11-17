;;; init-go.el -- setup go programming mode

;;; Commentary:

;;; Code:

(require-package 'go-mode)
(require-package 'company-go)

(add-hook 'go-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)   ; gofmt says use tabs
            (setq tab-width 4)
            (set (make-local-variable 'company-backends) '(company-go))))

(add-hook 'before-save-hook 'gofmt-before-save)

(provide 'init-go)

;;; init-go.el ends here
