;;; init-site-lisp.el -- load local versions of packages

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (loop for dir in (directory-files parent-dir)
                   unless (string-match "^\\." dir)
                   collecting (expand-file-name dir))
             load-path)))))

(sanityinc/add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

(provide 'init-site-lisp)

;;; init-site-lisp.el ends here
