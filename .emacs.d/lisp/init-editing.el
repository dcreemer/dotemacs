;;; init-editing.el -- basic editing setup

;;; Commentary:

;;; Code:

(setq-default c-basic-offset 4          ; prog indent is 4 chars
              tab-width 8               ; tabs are 8 characters
              indent-tabs-mode nil      ; but never inserted
              delete-selection-mode t
              scroll-preserve-screen-position 'always
              sentence-end-double-space nil
              require-final-newline t)

(dolist (hook '(special-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

(after-load 'subword
  (diminish 'subword-mode))

;; UTF-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-diff-options "-w"
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; ace-jump
(require-package 'ace-jump-mode)
(global-set-key (kbd "M-j") 'ace-jump-mode)

;; key preferences
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key '[(meta kp-delete)] 'kill-word)
(defun dc/join-forward ()
  "Join the next line to the current one."
  (interactive)
  (join-line 1))

(global-set-key (kbd "C-c J") 'dc/join-forward)

;; turn on some disabled commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer     'disabled nil)

;; aggressive!
(require-package 'aggressive-indent)
(global-aggressive-indent-mode 1)
(diminish 'aggressive-indent-mode)

;; expand region
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple-cursors
(require-package 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(setq mc/list-file (state-file "mc-lists.el"))

;; smartparens
;; I don't like the auto-escaping of quotes in quotes
(require-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode 1)
;; smartparens provides it's own show-parens-mode
(show-smartparens-global-mode t)
(setq sp-autoescape-string-quote nil)
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
(diminish 'smartparens-mode)

;; hilight etc.
(require-package 'highlight-escape-sequences)
(hes-mode)

(require-package 'fill-column-indicator)
;; (turn-on-fci-mode)

(provide 'init-editing)

;;; init-editing.el ends here
