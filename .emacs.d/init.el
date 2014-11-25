;;; init.el -- dcreemer's emacs file
;;
;;; Commentary:
;; see README.md for info and credits

;;; Code:

;; locally developed (or stolen) lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-mac-gui* (and (display-graphic-p) *is-a-mac*))

;; bootstrap
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-exec-path)

;; various package configurations
(require-package 'diminish)
(require 'init-ui)
(require 'init-osx-keys)
(require 'init-dired)
(require 'init-isearch)
(require 'init-ack)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-recentf)
(require 'init-ido)
(require 'init-hippie-expand)
(require 'init-company)
(require 'init-windows)
(require 'init-sessions)

(require 'init-shell)
(require 'init-tramp)
(require 'init-editing)
(require 'init-git)
(require 'init-github)

(require 'init-markdown)
(require 'init-org)
(require 'init-prog)
(require 'init-projectile)
(require 'init-nxml)
(require 'init-java)
(require 'init-python)
(require 'init-sql)
(require 'init-lisp)
(require 'init-clojure)
(require 'init-go)
(require 'init-spelling)
(require 'init-dash)
(require 'init-regex-tool)
(require 'init-web)

;; no customization needed
(require-package 'php-mode)
(require-package 'yaml-mode)

;; load private configuration when it exists
;; and also load other packages that depend on it
(load (private-file "private.el") 'noerror)

;; start emacs server
(require 'server)
(when (and (display-graphic-p)
           (not (server-running-p)))
  (server-start))

;; customizations go in a separate file
(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
      'noerror)

(provide 'init)

;;; init.el ends here
