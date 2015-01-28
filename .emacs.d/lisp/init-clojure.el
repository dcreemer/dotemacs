;;; init-clojure.el -- Clojure specific settings

;;; Commentary:

;;; Code:

(require-package 'cider)
(require-package 'clojure-mode)
;; (require-package 'clj-refactor)

(after-load 'cider-mode
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))

(after-load 'clojure-mode
  (add-hook 'clojure-mode-hook (lambda ()
                                 ;; (clj-refactor-mode 1)
                                 ;; (cljr-add-keybindings-with-prefix "C-c C-m")
                                 ;; (diminish 'clj-refactor-mode)
                                 (prettify-symbols-mode)
                                 (clojure-enable-cider)
                                 (diminish 'eldoc-mode))))


(provide 'init-clojure)

;;; init-clojure.el ends here
