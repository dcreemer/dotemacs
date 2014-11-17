;;; init-dired.el -- setup dired/dired+

;;; Commentary:

;;; Code:

(require-package 'dired+)
(require-package 'dired-sort)

(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

(after-load 'dired
  (require 'dired)
  (require 'dired+)
  (require 'dired-sort)
  (setq dired-use-ls-dired nil
        dired-recursive-deletes 'top))

(provide 'init-dired)

;;; init-dired.el ends here
