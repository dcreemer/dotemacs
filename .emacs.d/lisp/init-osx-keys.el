;;; init-osx-keys.el -- mac os x specific settings

;;; Commentary:

;; Code:

(when *is-a-mac*
  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (kbd (concat "<" multiple "wheel-" direction ">")) 'ignore))))

(provide 'init-osx-keys)

;;; init-osx-keys.el ends here
