;;; init-dash.el -- mac doc browser

;;; Commentary:

;;; Code:

(defun sanityinc/dash-installed-p ()
  "Return t if Dash is installed on this machine, or nil otherwise."
  (let ((lsregister "/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister"))
    (and (file-executable-p lsregister)
         (not (string-equal
               ""
               (shell-command-to-string
                (concat lsregister " -dump|grep com.kapeli.dash")))))))

(when (and *is-a-mac* (not (package-installed-p 'dash-at-point)))
  (message "Checking whether Dash is installed")
  (when (sanityinc/dash-installed-p)
    (require-package 'dash-at-point)
    (add-to-list 'dash-at-point-mode-alist '(python-mode . "python2,django,gevent"))))

(when (package-installed-p 'dash-at-point)
  (global-set-key (kbd "C-c D") 'dash-at-point-with-docset))

(provide 'init-dash)

;;; init-dash.el ends here
