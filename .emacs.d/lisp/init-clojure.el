;;; init-clojure.el -- Clojure specific settings

;;; Commentary:

;;; Code:

(require-package 'cider)
(require-package 'clojure-mode)
;; (require-package 'clj-refactor)

(after-load 'cider-mode
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'clojure-enable-cider))

(after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'prettify-symbols-mode)
  (add-hook 'clojure-mode-hook (lambda () (diminish 'eldoc-mode))))

(provide 'init-clojure)

;;; init-clojure.el ends here
