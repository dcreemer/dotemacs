;;
;; dcreemer's emacs file
;;

(defconst dc/macosx-gui-p
  (and window-system (eq 'darwin system-type)))

;; customizations go in a separate file:
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; some packages are forked or copied locally. find those here:
(let ((default-directory (expand-file-name "local-lisp/" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;
;; packages to load and ensure are found in the .emacs.d/Cask file
;;

(require 'cask "/usr/local/Cellar/cask/0.7.0/cask.el")
(cask-initialize)
(require 'pallet)

;; fix the PATH variable on Mac OS X gui
(when dc/macosx-gui-p
  (exec-path-from-shell-initialize))

;;
;; load custom functions
;;
(load-file (expand-file-name "util.el" user-emacs-directory))

;;
;; save backups to common location, and keep more versions
;;
(setq backup-directory-alist `(("." . ,(state-file "backups"))))
(setq delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t)

(setq auto-save-list-file-prefix (state-file "auto-save-list/.saves-"))

;;
;; save and restore sessions
;;
(setq savehist-file (state-file "history"))
(setq savehist-additional-variables '(kill-ring))
(savehist-mode 1)

(elscreen-start)
(setq elscreen-tab-display-control nil
      elscreen-display-screen-number nil)

(setq desktop-path (list user-state-directory)
      desktop-base-file-name "emacs.desktop"
      desktop-restore-frames t)

(add-hook 'desktop-save-hook 'elscreen-store-hook)
(add-hook 'desktop-after-read-hook 'elscreen-load-hook)

(desktop-save-mode 1)

;;
;; simple auto-projects
;;
(setq projectile-cache-file (state-file "projectile.cache")
      projectile-known-projects-file (state-file "projectile-bookmarks.eld"))
(projectile-global-mode)

;;
;; Magit
;;
(global-set-key "\C-xg" 'magit-status)

;;
;; UI Settings
;;

;; Basic global keys
(global-set-key "\M- " 'hippie-expand)
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key '[(meta kp-delete)] 'kill-word)

;; better directory traversal
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-save-directory-list-file (state-file "ido.last"))
(ido-vertical-mode 1)
(setq ido-use-virtual-buffers t)

;;
;; smarter meta-x
(require 'smex)
(smex-initialize)
(setq smex-save-file (state-file "smex-items"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; return to same point in a buffer when revisiting the file:
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (state-file "places"))

;; remember many recent files:
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15
      recentf-save-file (state-file "recentf"))
(recentf-mode +1)
(global-set-key (kbd "C-c f") 'recentf-ido-find-file)

;; ensure all buffers have unique names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; turn on menu-, off tool-, and scroll-bars
(if window-system
    (menu-bar-mode 1)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; set the color theme to something nice
(require 'color-theme)
(color-theme-initialize)
(load-theme 'cyberpunk t)
(when window-system
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-14"))

;; I need to edit some very large YAML files. Maximum font-lock slows that down
(setq font-lock-maximum-decoration '((yaml-mode . 1) (t . t)))

;; always show empty space at end of buffer and line
(set-default 'indicate-empty-lines t)
(setq show-trailing-whitespace t)

;; Use shift key to navigate windows:
(windmove-default-keybindings 'shift)

;; make the cursor more visible:
;(global-hl-line-mode)

;; fill column is 1/2 full screen w/with two side-by-side windows on my mac:
(set-default 'fill-column 95)

;; turn on some disabled commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer     'disabled nil)

;; shell -- currently favoring eshell
;;(global-set-key (kbd "C-$") '(lambda () (interactive) (ansi-term "bash")))
(global-set-key (kbd "C-$") 'eshell)
(setq eshell-directory-name (state-file "eshell"))
(setq eshell-aliases-file (expand-file-name "eshell-aliases" user-emacs-directory))

;;
;; spelling
;;
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;
;; log files auto 'tail -f'
;;
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;;
;; expand region
;;
(global-set-key (kbd "C-=") 'er/expand-region)

;;
;; multiple-cursors
;;
(require 'multiple-cursors)
;; suggested defaults from https://github.com/magnars/multiple-cursors.el
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(setq mc/list-file (state-file "mc-lists.el"))

;;
;; Org
;;
(setq org-startup-indented t
      org-startup-folded "showall")

;;
;; Code Formatting Globally:
;;

;; tabs are 8 charachters, but we never generate them.
(set-default 'c-basic-offset 4)
(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 8)

(show-paren-mode 1)
(column-number-mode)
(global-rainbow-delimiters-mode)

;; smart parens everywhere, but I don't like the auto-escaping of quotes in quotes
(require 'smartparens-config)
(smartparens-global-mode 1)
(setq sp-autoescape-string-quote nil)
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

;; auto-completion everywhere
(require 'auto-complete-config)
(ac-config-default)
(setq ac-comphist-file (state-file "ac-comphist.dat"))
(global-auto-complete-mode t)

;; highlight-synbols in all programming modes
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;; yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook 'yas-minor-mode)

;; markdown
(add-hook 'markdown-mode-hook 'yas-minor-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;;
;; clojure
;;
(add-hook 'clojure-mode-hook 'subword-mode) ; allow for CamelCase
(add-hook 'clojure-mode-hook 'auto-complete-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;;
;; Go
;;
(add-hook 'go-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)   ; gofmt says use tabs
            (setq tab-width 4)))

;;
;; YAML
;;
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;
;; Python
;;
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (setq jedi:complete-on-dot t)
            (local-set-key "\C-c\C-d" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (setq jedi:use-shortcuts t))) ; M-. and M-,

;;
;; w3m
;;
;(setq browse-url-browser-function 'w3m-browse-url)
(setq w3m-default-display-inline-images t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(global-set-key "\C-xw" 'w3m-browse-url)

;;
;; smart-mode-line
;;
(setq sml/theme 'automatic)
(setq sml/hidden-modes '(" SP" " hl-p" " hl-s"))
(sml/setup)

;;
;; in-emacs pandora client. yay.
;;
(require 'pianobar)
(global-set-key (kbd "<M-f7>") 'pianobar-ban-song)
(global-set-key (kbd "<f8>") 'pianobar-play-or-pause)
(global-set-key (kbd "<M-f8>") '(lambda () (interactive) (pianobar-send-command ?q)))
(global-set-key (kbd "<f9>") 'pianobar-next-song)
(global-set-key (kbd "<M-f9>") 'pianobar-change-station)

;;
;; twitter
;;
(require 'twittering-mode)
(setq twittering-icon-mode t
      twittering-use-icon-storage t
      twittering-icon-storage-file "~/.emacs.d/state/twittering-mode-icons"
      twittering-cert-file (cond
                            ((string= system-type "darwin") "/usr/local/etc/openssl/cert.pem")
                            ((string= system-type "berkeley-unix") "/usr/local/share/certs/ca-root-nss.crt")
                            (t nil)))

;;
;; bitlbee / erc
;;
(require 'erc-terminal-notifier)

(defun dc/chat ()
  "start local chat proxy"
  (interactive)
  (erc :server "localhost" :port 6667 :nick bitlbee-erc-username :password bitlbee-erc-password))

;;
;; load private configuration
;;
;; add the private local list directory to load path
(let ((private-config-file (expand-file-name "private/private.el" user-emacs-directory)))
  (when (file-exists-p private-config-file)
    (load-file private-config-file)))

;;
;; start emacs server
;;
(when window-system
  (server-start))
