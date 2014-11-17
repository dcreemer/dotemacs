;;; init-company.el -- settings for company auto-complete mode

;;; Commentary:

;;; Code:

(require-package 'company)

(setq company-tooltip-limit 20                      ; bigger popup window
      company-idle-delay .2                         ; decrease delay before autocompletion popup shows
      company-echo-delay 0)                         ; remove annoying blinking

(add-hook 'prog-mode-hook
          (lambda ()
            (company-mode 1)
            (diminish 'company-mode)
            (define-key (current-local-map) (kbd "M-SPC") 'company-complete)))

(provide 'init-company)

;;; init-company.el ends here
