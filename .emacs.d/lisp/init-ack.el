;;; init-ack.el -- setup ack, grep, and other searching tools

;;; Commentary:

;;; Code:

;; ack -> ack-and-a-half
(require-package 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(provide 'init-ack)

;;; init-ack.el ends here
