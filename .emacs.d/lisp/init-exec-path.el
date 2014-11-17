;;; init-exec-path -- set the exec path and other env vars

;;; Commentary:
;; on mac os X GUI, set the exec path from the shell variables
;; as they are not picked up from the parent process

;;; Code:

(require-package 'exec-path-from-shell)

(after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "GOPATH"))
    (add-to-list 'exec-path-from-shell-variables var)))

(when *is-mac-gui*
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)

;;; init-exec-path.el ends here
