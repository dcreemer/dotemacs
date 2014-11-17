;;; init-ack.el -- setup ack, grep, and other searching tools

;;; Commentary:

;;; Code:

;; ack -> ack-and-a-half
(require-package 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)

(setq-default grep-highlight-matches t
              grep-scroll-output t)

;; silver searcher as a replacement for ack/grep
(require-package 'ag)
(setq ag-reuse-buffers t
      ag-highlight-search t)

(after-load 'ag
  (add-to-list 'ag-arguments "--hidden"))

(add-hook 'ag-mode-hook
          (lambda ()
            (copy-face 'lazy-highlight 'ag-match-face)))

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(provide 'init-ack)

;;; init-ack.el ends here
