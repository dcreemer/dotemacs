;;; init-tramp.el -- setup remote editing

;;; Commentary:

;;; Code:

(require 'tramp)
(setq tramp-default-method "ssh"
      ;; prevent tramp from attempting to figure out VC status
      vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))

(provide 'init-tramp)

;;; init-tramp.el ends here
