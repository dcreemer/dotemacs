;;; init-projectile.el -- simple auto-projects
;;
;;; Commentary:

;;; Code:

(require-package 'projectile)

(setq projectile-cache-file (state-file "projectile.cache")
      projectile-known-projects-file (state-file "projectile-bookmarks.eld")
      projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

(projectile-global-mode)

(provide 'init-projectile)

;;; init-projectile.el ends here