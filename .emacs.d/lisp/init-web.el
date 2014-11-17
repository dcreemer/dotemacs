;;; init-web.el -- web development and browsing

;;; Commentary:
;; everything needed for web browsing and (limited) development

;;; Code:

(require-package 'w3m)
(setq w3m-default-display-inline-images t)
;; (setq browse-url-browser-function 'w3m-browse-url)

(require-package 'web-mode)
;; temaplates
(add-auto-mode 'web-mode "\\.html$")
(add-auto-mode 'web-mode "\\.mustache$")

(provide 'init-web)

;;; init-web.el ends here
