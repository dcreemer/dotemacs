;;; init-clojure.el -- Clojure specific settings

;;; Commentary:

;;; Code:

(require-package 'cider)
(require-package 'clojure-mode)
(require-package 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")
                               (diminish 'clj-refactor-mode)
                               (subword-mode 1)
                               (prettify-symbols-mode)))
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)

(provide 'init-clojure)

;;; init-clojure.el ends here
