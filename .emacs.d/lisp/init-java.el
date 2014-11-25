;;; init-java.el -- java programming

;;; Commentary:
;;

;;; Code:

;; ggtags
(require 'ggtags)
(add-hook 'java-mode-hook 'ggtags-mode)

(provide 'init-java)

;;; init-java.el ends here
