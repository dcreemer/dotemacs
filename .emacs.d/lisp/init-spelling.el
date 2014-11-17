;;; init-spelling.el -- setup spell checking

;;; Commentary:

;;; Code:

(require 'ispell)

(when (executable-find ispell-program-name)
  (require 'flyspell)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (diminish 'flyspell-mode))

(provide 'init-spelling)

;;; init-spelling.el ends here
