;;; init-ui.el -- user interface related settings

;;; Commentary:

;;; Code:

(when *is-mac-gui*
  (setq ns-use-srgb-colorspace t))

;; turn on menu-, off tool-, and scroll-bars
(if (display-graphic-p)
    (menu-bar-mode 1)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; reduce prompts
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; set the color theme to something nice
(require-package 'color-theme)
(color-theme-initialize)

(require-package 'cyberpunk-theme)
(load-theme 'cyberpunk t)

;; DejaVu Sans Mono
(when (and (display-graphic-p)
           (find-font (font-spec :name "DejaVu Sans Mono")))
  (set-frame-font "DejaVu Sans Mono-12" t t))

;; set frame title to full path of file:
(when (display-graphic-p)
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b")))))

;; blinky blinky
(when (display-graphic-p)
  (setq-default cursor-type 'box
                blink-cursor-delay 0.4
                blink-cursor-interval 0.4)
  (blink-cursor-mode 1))

;; echo keystrokes right away
(setq-default echo-keystrokes 0.1)

;; no bell
(setq-default visible-bell t)

;; wrap rather than truncate lines
(setq-default truncate-lines nil)

;; always show whitespace
(require 'whitespace)
(setq whitespace-line-column 100
      whitespace-style
      '(face empty lines-tail spaces space-mark tabs tab-mark trailing))

;; always show column numbers
(setq-default column-number-mode t)

;; I need to edit some very large YAML files. Maximum font-lock slows that down
(setq font-lock-maximum-decoration '((yaml-mode . 1) (t . t)))

;; always show empty space at end of buffer and line
(setq indicate-empty-lines t)
(setq show-trailing-whitespace t)

;; make the cursor more visible:
(global-hl-line-mode)

;; fill column is 1/2 full screen w/with two side-by-side windows on my mac:
(set-default 'fill-column 95)

;; guide-key
;; popup a window with key completions-annotations
(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x n" "C-c !" "C-c p" "C-c p s"))
(guide-key-mode 1)
(diminish 'guide-key-mode)

;; maybe move:
(require-package 'alert)
(setq-default compilation-scroll-output t)

;; fullscreen
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

;; resize windows to according to golden ratio
(require-package 'golden-ratio)

(provide 'init-ui)

;;; init-ui.el ends here
