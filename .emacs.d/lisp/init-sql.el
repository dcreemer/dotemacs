;;; init-sql.el -- customize SQL handling

;;; Commentary:

;;; Code:

(require-package 'sql-indent)
(after-load 'sql
  (require 'sql-indent))

(setq-default sql-input-ring-file-name (state-file ".sqli_history"))

(after-load 'page-break-lines
  (push 'sql-mode page-break-lines-modes))

(require 'ggtags)
(add-hook 'sql-mode-hook 'ggtags-mode)

(provide 'init-sql)

;;; init-sql.el ends here
