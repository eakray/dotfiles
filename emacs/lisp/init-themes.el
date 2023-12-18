;;;; Appearance

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

(use-package color-theme-sanityinc-solarized)
(use-package gruvbox-theme)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard t))

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(gruvbox-dark-hard))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;; Toggle between light and dark
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

(defun gruv ()
  "Activate the gruvbox dark hard theme."
  (interactive)
  (setq custom-enabled-themes '(gruvbox-dark-hard))
  (reapply-themes))

(setq default-frame-alist
      '((width . 120)
        (height . 40)
        (top . 0)
        ; calculate `left` to center the frame
        (left . 300)))  ; center the frame

(use-package dimmer)
(setq-default dimmer-fraction 0.25)
(add-hook 'after-init-hook 'dimmer-mode)
(with-eval-after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
    (with-eval-after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun sanityinc/display-non-graphic-p ()
        (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p))

;; setting default font (get font working correctly)
(add-to-list 'default-frame-alist
	     '(font . "Comic Mono-16"))

(provide 'init-themes)
;;; init-themes.el ends here