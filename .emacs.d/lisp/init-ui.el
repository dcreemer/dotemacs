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
(require-package 'oldlace-theme)
(load-theme 'cyberpunk t)
;;(load-theme 'oldlace t)
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-13"))

;; I need to edit some very large YAML files. Maximum font-lock slows that down
(setq font-lock-maximum-decoration '((yaml-mode . 1) (t . t)))

;; always show empty space at end of buffer and line
(setq indicate-empty-lines t)
(setq show-trailing-whitespace t)

;; make the cursor more visible:
(global-hl-line-mode)

;; fill column is 1/2 full screen w/with two side-by-side windows on my mac:
(set-default 'fill-column 95)

;; maybe move:
(require-package 'alert)
(setq-default compilation-scroll-output t)

(provide 'init-ui)

;;; init-ui.el ends here
