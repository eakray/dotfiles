;; basic quality of life settings
(setq inhibit-startup-screen t)
(setq use-file-dialog nil)
(scroll-bar-mode -1)
(recentf-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-fringe-mode 12)
(setq visible-bell t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil)
(setq initial-scratch-message nil)

;; get value of env var PATH
(getenv "PATH")

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;;; Line numbers

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 2)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; line column settings
;;(column-number-mode)
;; Enable line numbers for some modes
;;(dolist (mode '(text-mode-hook
;;                prog-mode-hook
;;                conf-mode-hook))
;;  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
;;(dolist (mode '(org-mode-hook))
;;  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(provide 'init-config)
;;; init-config.el ends here