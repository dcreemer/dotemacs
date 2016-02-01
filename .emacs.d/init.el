;;; init.el -- dcreemer's emacs file
;;
;;; Commentary:
;; see README.md for info and credits

;;; Code:


;; -----------------------------------------------------------------------------
;; bootstrap the package system
;; -----------------------------------------------------------------------------


(require 'package)

;; repositories
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; I use 'use-package' to manage and configure packages

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; silence warnings of redefinitions due to defadvice
(setq ad-redefinition-action 'accept)

(eval-when-compile
  (require 'use-package))

;; log everything use-package does
(setq use-package-verbose t)

;; always check (unless otherwise noted) for installed packckages
(setq use-package-always-ensure t)

;; required by the use-package macro
(require 'diminish)
(require 'bind-key)

;; add homebrew site-lisp if present
(let ((default-directory "/usr/local/share/emacs/site-lisp"))
  (when (file-exists-p default-directory)
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))


;; -----------------------------------------------------------------------------
;; define the state of the system
;; -----------------------------------------------------------------------------


(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-mac-gui* (and (display-graphic-p) *is-a-mac*))

;; Mac OS X Emacs.app needs a bit of help getting shell variables
(use-package exec-path-from-shell
  :if *is-mac-gui*
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "GOPATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; use a standard tmp file location
(when *is-a-mac*
  (put 'temporary-file-directory 'standard-value '((file-name-as-directory "/tmp"))))

;; keep transient state in a custom directory
(defvar user-state-directory
  (expand-file-name "state" user-emacs-directory)
  "Default directory for transient user state.")

(defun state-file (path)
  "Calculate the PATH for a transient state file."
  (expand-file-name path user-state-directory))

;; save backup files to common location, and keep more versions
(setq backup-directory-alist `(("." . ,(state-file "backups")))
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t)

;; auto-save also goes to state directory
(setq auto-save-list-file-prefix (state-file "auto-save-list/.saves-"))


;; -----------------------------------------------------------------------------
;; setup text editing and the user interface the way I like it.
;; -----------------------------------------------------------------------------


;; I know what program I'm using
(setq inhibit-startup-screen t)

(when *is-mac-gui*
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (global-set-key (kbd "s-c") #'clipboard-kill-ring-save)
  (global-set-key (kbd "s-v") #'clipboard-yank))

;; turn on menu-, off tool-, and scroll-bars
(if (display-graphic-p)
    (menu-bar-mode 1)
  (menu-bar-mode -1))

;; tool-bar off
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; scroll-bar off
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; reduce prompts
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; set the color theme to something nice
(use-package color-theme
  :defer 2
  :config
  (color-theme-initialize)
  (use-package cyberpunk-theme
    :config
    (load-theme 'cyberpunk t)))

;; DejaVu Sans Mono
(when (and (display-graphic-p)
           (find-font (font-spec :name "DejaVu Sans Mono")))
  (set-frame-font "DejaVu Sans Mono-10" t t))

;; set frame title to full path of file:
(when (display-graphic-p)
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b")))))

;; blinky blinky
(when (display-graphic-p)
  (setq-default cursor-type 'box)
  (setq blink-cursor-delay 0.4)
  (setq blink-cursor-interval 0.4)
  (blink-cursor-mode 1))

;; echo keystrokes right away
(setq-default echo-keystrokes 0.1)

;; no bell
(setq-default visible-bell t)

;; wrap rather than truncate lines
(setq-default truncate-lines nil)

;; always show whitespace
(use-package whitespace
  :ensure nil
  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column 100)
  (setq whitespace-style '(face empty lines-tail spaces space-mark tabs tab-mark trailing)))

;; always show column numbers
(setq-default column-number-mode t)

;; always show empty space at end of buffer
(setq-default indicate-empty-lines t)

;; make the cursor more visible:
(global-hl-line-mode)

;; fill column is 1/2 full screen w/with two side-by-side windows on my mac:
(setq-default fill-column 100)

;; indent is 4 charactes
(setq-default c-basic-offset 4)

;; tabs are 8
(setq-default tab-width 8)

;; never insert tabs
(setq-default indent-tabs-mode nil)

;; it's nice to be able to overwrite the selection
(delete-selection-mode +1)

;; why isn't it always this?
(setq scroll-preserve-screen-position 'always)

;; old-school
(setq sentence-end-double-space nil)

;; I hate hanging last lines
(setq-default require-final-newline t)

;; UTF-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; return indents too
(define-key global-map (kbd "RET") #'newline-and-indent)

;; kill forward word
(global-set-key '[(meta kp-delete)] #'kill-word)

;; like join from vim.
(defun dc/join-forward ()
  "Join the next line to the current one."
  (interactive)
  (join-line 1))

(global-set-key (kbd "C-c J") #'dc/join-forward)

;; turn on some disabled commands
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-c m" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (setq mc/list-file (state-file "mc-lists.el")))


;; smartparens everywhere
(use-package smartparens
  :init
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  :diminish smartparens-mode
  :bind (("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp))
  :config
  (show-smartparens-global-mode t))


;; FCI off for now
(use-package fill-column-indicator
  :disabled t
  :defer t
  :config (turn-on-fci-mode))


;; which-key is great
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

(use-package hydra)

;; spelling
(use-package ispell
  :ensure nil
  :defer 2
  :config
  (use-package flyspell
    :if (executable-find ispell-program-name)
    :config
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)
    (diminish 'flyspell-mode)))

;; fullscreen
(global-set-key (kbd "<s-return>") #'toggle-frame-fullscreen)

(defhydra hydra-zoom ()
  "zoom"
  ("+" text-scale-increase "in")
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))
(global-set-key (kbd "C-x C-0") #'hydra-zoom/body)

;; -----------------------------------------------------------------------------
;; dired
;; -----------------------------------------------------------------------------

(use-package dired
  :ensure nil
  :defer t
  :config
  (use-package dired+
    :defer t
    :init
    (setq diredp-hide-details-initially-flag nil))
  (use-package dired-sort
    :defer t)
  (setq dired-dwim-target t)
  (setq dired-use-ls-dired nil)
  (setq dired-recursive-deletes 'top))


;; -----------------------------------------------------------------------------
;; in-buffer search and movement
;; -----------------------------------------------------------------------------


;; use "swiper" instead of isearch
(use-package swiper
 :bind (("C-r" . swiper)
        ("C-s" . swiper)))

;; ace-jump
(use-package ace-jump-mode
  :bind ("M-j" . ace-jump-mode))


;; -----------------------------------------------------------------------------
;; multi-file search
;; -----------------------------------------------------------------------------


;; use "selver-searcher" ("ag") instead of grep/ack
(use-package ag
  :defer t
  :init
  (setq ag-reuse-buffers t)
  (setq ag-highlight-search t)
  :config
  (add-to-list 'ag-arguments "--hidden")
  (add-hook 'ag-mode-hook
            (lambda ()
              (copy-face 'lazy-highlight 'ag-match-face))))

;; on a Mac, "mdfind" is better than "locate"
(when *is-a-mac*
  (setq-default locate-command "mdfind"))


;; -----------------------------------------------------------------------------
;; buffer management
;; -----------------------------------------------------------------------------


;; buffers should have unique names
(use-package uniquify
  :ensure nil
  :defer 2
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; setup ibuffer to look pretty
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :config
  ;; Explicitly load ibuffer-vc first to get its column definitions
  (use-package ibuffer-vc
    :config
    ;; Modify the default ibuffer-formats (toggle with `)
    (setq ibuffer-formats
          '((mark modified read-only vc-status-mini " "
                  (name 18 18 :left :elide)
                  " "
                  (size-h 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  filename-and-process)
            (mark modified read-only vc-status-mini " "
                  (name 18 18 :left :elide)
                  " "
                  (size-h 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  (vc-status 16 16 :left)
                  " "
                  filename-and-process))))
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  (defun ibuffer-set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))
  (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)
  (setq ibuffer-filter-group-name-face 'font-lock-doc-face))


;; -----------------------------------------------------------------------------
;; window and session management
;; -----------------------------------------------------------------------------


;; Use shift keys ...
(windmove-default-keybindings 'shift)

;; and M-J (via ace-window) to navigate windows
(use-package ace-window
  :bind ("M-J" . ace-window))

(use-package golden-ratio
  :bind ("M-G" . golden-ratio))

;; return to same point in a buffer when revisiting the file:
(use-package saveplace
  :ensure nil
  :config
  (setq-default save-place t)
  (setq save-place-file (state-file "places")))

;; save and restore minibuffer history
(use-package savehist-mode
  :ensure nil
  :demand t
  :init
  (setq savehist-file (state-file "history"))
  (setq savehist-additional-variables '(kill-ring))
  :config
  (savehist-mode 1))

;; remember files I have visited
(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 5)
  (setq recentf-save-file (state-file "recentf"))
  (setq recentf-exclude '("/tmp/" "/ssh:"))
  (recentf-mode +1))


;; -----------------------------------------------------------------------------
;; ok I'll try helm
;; -----------------------------------------------------------------------------

(use-package helm
  :defer nil
  :bind (("C-x b" . helm-mini)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-c h a" . helm-apropos)
         ("C-c h i" . helm-info-at-point)
         ("C-c h l" . helm-locate)
         ("C-c h m" . helm-man-woman)
         ("C-c h o" . helm-google-suggest)
         ("C-c h t" . helm-top))
  :diminish helm-mode
  :config
  (setq helm-locate-command (case system-type
                              ('gnu/linux "locate %s -e -A --regex %s")
                              ('berkeley-unix "locate %s %s")
                              ('windows-nt "es %s %s")
                              ('darwin "mdfind -name %s %s")
                              (t "locate %s %s")))
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  (helm-mode +1)
  (use-package helm-descbinds
    :bind ("C-c h b" . helm-descbinds))
  (use-package helm-projectile) ; C-c p h
  (use-package helm-ag
    :bind ("C-c h g" . helm-ag))
  (use-package helm-swoop
    :bind ("C-c h s" . helm-swoop))
  (use-package helm-dash
    :bind ("C-c h d" . helm-dash-at-point)
    :config
    (setq helm-dash-browser-func 'eww ; good enough
          helm-dash-docsets-path (state-file "docsets")
          helm-dash-common-docsets '("emacs"))))

;; TODO: look into:
;; https://github.com/emacs-helm/helm-mu


;; -----------------------------------------------------------------------------
;; Dynamic expansion
;; -----------------------------------------------------------------------------


(use-package hippie-expand
  :ensure nil
  :diminish abbrev-mode
  :bind (("M-SPC" . hippie-expand)
         ("M-/" . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill)))


;; -----------------------------------------------------------------------------
;; Org and friends
;; -----------------------------------------------------------------------------


;; I store my notes here:
(defvar dc/notes-dir
  "~/Documents/Notes"
  "Directory for soring and finding notes.")

;; deft for managing org notes
(use-package deft
  :bind ("C-c d" . deft)
  :config
  (setq deft-directory dc/notes-dir)
  (setq deft-extensions '("org" "txt" "text" "md" "markdown"))
  (setq deft-use-filename-as-title t)
  (setq deft-default-extension "org")
  ;; (setq deft-use-filename-as-title t)
  ;; (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 5.0))

;; everyone loves org-mode
(use-package org
  :bind (("\C-c a" . org-agenda)
         ("\C-c l" . org-store-link)
         ("\C-c c" . org-capture))
  :config
  (setq org-startup-indented t)
  (setq org-startup-folded "content")
  (setq org-agenda-files (list dc/notes-dir))
  (setq org-default-notes-file (concat dc/notes-dir "/inbox.org"))
  (setq org-refile-targets '((org-agenda-files . (:tag . "refile"))))
  (setq org-outline-path-complete-in-steps t)
  (setq org-refile-use-outline-path t)
  (org-babel-do-load-languages 'org-babel-load-languages '((sh . t) (python . t)))
  (add-hook 'org-mode-hook #'auto-fill-mode))

;; epresent for presentations
(use-package epresent
  :commands epresent-run)

(defhydra hydra-org (:color red :columns 3)
  "Org Mode Movements"
  ("n" outline-next-visible-heading "next heading")
  ("p" outline-previous-visible-heading "prev heading")
  ("N" org-forward-heading-same-level "next heading at same level")
  ("P" org-backward-heading-same-level "prev heading at same level")
  ("u" outline-up-heading "up heading")
  ("g" org-goto "goto" :exit t))
(global-set-key (kbd "C-c o") #'hydra-org/body)

;; todo.txt for todo list management
(use-package todotxt
  :bind ("\C-c t" . todotxt)
  :config
  (setq todotxt-file "~/Documents/Todo/todo.txt"))


;; -----------------------------------------------------------------------------
;; terminal configuration
;; -----------------------------------------------------------------------------


;; http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/
(use-package multi-term
  :bind (("C-$" . multi-term)
         ("s-{" . multi-term-prev)
         ("s-}" . multi-term-next))
  :config
  (setq multi-term-buffer-name "term")
  (setq multi-term-program "/bin/bash"))

(when (require 'term nil t)
  (setq term-bind-key-alist
        (list (cons "C-c C-c" #'term-interrupt-subjob)
              (cons "C-p" #'previous-line)
              (cons "C-n" #'next-line)
              (cons "M-f" #'term-send-forward-word)
              (cons "M-b" #'term-send-backward-word)
              (cons "C-c C-j" #'term-line-mode)
              (cons "C-c C-k" #'term-char-mode)
              (cons "M-DEL" #'term-send-backward-kill-word)
              (cons "M-d" #'term-send-forward-kill-word)
              (cons "<C-left>" #'term-send-backward-word)
              (cons "<C-right>" #'term-send-forward-word)
              (cons "C-r" #'term-send-reverse-search-history)
              (cons "M-p" #'term-send-raw-meta)
              (cons "M-y" #'term-send-raw-meta)
              (cons "C-y" #'term-send-raw))))

(use-package tramp
  :ensure nil
  :defer t
  :config
  (setq tramp-default-method "ssh")
  ;; prevent tramp from attempting to figure out VC status
  (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)))


;; -----------------------------------------------------------------------------
;; web browsing
;; -----------------------------------------------------------------------------


(use-package w3m
  :commands w3m
  :config
  ;; (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-default-display-inline-images t))


;; -----------------------------------------------------------------------------
;; General programming configuration begins here
;; -----------------------------------------------------------------------------


;; use flycheck everywhere we can
(use-package flycheck
  :defer t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  ;; Override default flycheck triggers
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 3.0
        flycheck-flake8-maximum-line-length 100
        flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))


;; try to do company-based completion everywhere
(use-package company
  :defer t
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  :config
  ;; bigger popup window
  (setq company-tooltip-limit 20
        ;; decrease delay before autocompletion popup shows
        company-idle-delay .2
        ;; remove annoying blinking
        company-echo-delay 0)
  (add-hook 'prog-mode-hook (lambda () (define-key (current-local-map) (kbd "M-SPC") #'company-complete))))


;; highlight-symbols in all programming modes
(use-package highlight-symbol
  :defer t
  :diminish highlight-symbol-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode))


;; rainbow parens!
(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; show indents guides
(use-package indent-guide
  :defer t
  :diminish indent-guide-mode
  :init
  (add-hook 'prog-mode-hook #'indent-guide-mode))


;; gnu global -- ggtags
(use-package ggtags
  :defer t
  :config
  (add-hook 'ggtags-mode-hook (lambda ()
                                (global-set-key (kbd "M->") 'end-of-buffer)
                                (global-set-key (kbd "M-<") 'beginning-of-buffer))))


;; Go, Java, etc. like subwords
(use-package subword
  :ensure nil
  :defer 2
  :diminish subword-mode)


;; Dash to lookup documentation on Mac OS X (though see helm-dash)
(use-package dash-at-point
  :if *is-mac-gui*
  :bind ("C-c D" . dash-at-point)
  :config
  (add-to-list 'dash-at-point-mode-alist '(python-mode . "python2,django,gevent")))


;; playing with regular expressions
(use-package regex-tool
  :commands regex-tool
  :config
  (setq regex-tool-backend 'Perl))


;; -----------------------------------------------------------------------------
;; diff, VC, git, github, projects
;; -----------------------------------------------------------------------------


(use-package ediff
  :ensure nil
  :commands ediff
  :config
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-diff-options "-w"))

;; Magit is amazing...
(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  (use-package git-commit
    :defer t)
  :config
  (use-package magit-gh-pulls
    :defer t)
  (setq magit-process-popup-time 10
        magit-diff-refine-hunk t))


;; show git line status in gutter
(use-package git-gutter
  :bind ("C-x C-g" . git-gutter:toggle)
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :defer t)

(setq vc-follow-symlinks t)

(use-package github-browse-file
  :defer t)

(use-package projectile
  :config
  (setq projectile-cache-file (state-file "projectile.cache")
        projectile-known-projects-file (state-file "projectile-bookmarks.eld")
        projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  (projectile-global-mode)
  (define-key projectile-mode-map [remap projectile-grep] #'projectile-ag))

(use-package ranger
  :commands ranger)

;; -----------------------------------------------------------------------------
;; Major editing modes
;; -----------------------------------------------------------------------------


;; markdown
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config
  (add-hook 'markdown-mode-hook #'auto-fill-mode)
  (add-hook 'markdown-mode-hook #'fci-mode)
  (add-hook 'markdown-mode-hook '(lambda () (setq-local helm-dash-docsets '("Markdown")))))

;; XML
(use-package nxml-mode
  :ensure nil
  :mode ("\\.xml\\'" "\\.rss\\'" "\\.plist\\'")
  :init
  (setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
  (fset 'xml-mode 'nxml-mode)
  :config
  (setq nxml-slash-auto-complete-flag t))

(defun nxml-pretty-buffer ()
  "Reformat the XML in the whole buffer."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region 0 (count-lines (point-min) (point-max)))))

;; JSON
(use-package json-mode
  :mode "\\.json\\'")

;; SQL
(use-package sql
  :ensure nil
  :defer t
  :init
  (add-hook 'sql-mode-hook '(lambda () (setq-local helm-dash-docsets '("MySQL"))))
  :config
  (use-package sql-indent)
  (setq-default sql-input-ring-file-name (state-file ".sqli_history")))

;; PHP
(use-package php-mode
  :mode "\\.php\\'")

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; Java
(add-hook 'java-mode-hook 'ggtags-mode)

;; Python
(use-package python-mode
  :mode "\\.py\\'"
  :init
  (add-hook 'python-mode-hook #'eldoc-mode)
  (add-hook 'python-mode-hook '(lambda () (setq-local helm-dash-docsets '("Python 2"))))
  :config
  (setq flycheck-flake8-maximum-line-length 100))

(use-package pip-requirements
  :mode (("\\.pip\\'" . pip-requirements-mode)
         ("requirements.*\\.txt\\'" . pip-requirements-mode)))

(use-package anaconda-mode
  :defer t
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  :config
  (use-package company-anaconda
    :config
    (add-to-list 'company-backends #'company-anaconda)))
;;  (define-key anaconda-mode-map (kbd "M-,") 'anaconda-nav-pop-marker))

;; Emacs-lisp
(use-package elisp-slime-nav
  :defer t
  :diminish elisp-slime-nav-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode))

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook '(lambda () (setq-local helm-dash-docsets '("Emacs Lisp"))))

;; Clojure
(use-package cider
  :defer t
  :init
  (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package clojure-mode
  :defer t
  :config
  (add-hook 'clojure-mode-hook #'prettify-symbols-mode)
  (add-hook 'clojure-mode-hook #'clojure-enable-cider))

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :config
  (use-package company-go)
  (use-package go-eldoc
    :config
    (add-hook 'go-mode-hook #'go-eldoc-setup))
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local indent-tabs-mode t) ; gofmt says use tabs...
              (setq-local tab-width 4)        ; which are 4 chars...
              (ggtags-mode 1)
              (whitespace-mode 0)))
  (add-hook 'go-mode-hook '(lambda () (setq-local helm-dash-docsets '("Go"))))
  (use-package go-direx
    :config
    (define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer))
  (add-hook 'before-save-hook #'gofmt-before-save))

;; Shell
(add-hook 'sh-mode-hook '(lambda () (setq-local helm-dash-docsets '("Bash"))))


;; -----------------------------------------------------------------------------
;; Other...
;; -----------------------------------------------------------------------------

(use-package fullframe
  :config
  (fullframe list-packages quit-window)
  (fullframe ibuffer ibuffer-quit))


(use-package restclient
  :mode "\\.http\\'")


;; Rename the current file
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

;; insert current UTC timestamp
(defun insert-utc ()
  "Insert the current UTC date and time ast ISO8601."
  (interactive)
  (let ((now (current-time)))
    (set-time-zone-rule t)
    (insert (format-time-string "%FT%TZ" now))
    (set-time-zone-rule nil)))


;; -----------------------------------------------------------------------------
;; private data
;; -----------------------------------------------------------------------------


;; I keep my private configs in a separate repo mapped here:
(defvar user-private-directory
  (expand-file-name "private" user-emacs-directory)
  "Default directory for private data and code.")

(defun private-file (path)
  "Calculate the PATH for a private file."
  (expand-file-name path user-private-directory))


;; load private configuration when it exists
;; and also load other packages that depend on it
(load (private-file "private.el") 'noerror)


;; -----------------------------------------------------------------------------
;; server
;; -----------------------------------------------------------------------------


(use-package server
  :ensure nil
  :demand t
  :config
  (when (and (display-graphic-p) (not (server-running-p)))
    (server-start)))


;; -----------------------------------------------------------------------------
;; UI-based customizations
;; -----------------------------------------------------------------------------

;; customizations go in a separate file
(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
       'noerror)

(provide 'init)

;;; init.el ends here
