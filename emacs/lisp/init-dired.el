;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package diredfl)
(add-hook 'dired-mode-hook 'diredfl-mode)

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(setq dired-recursive-deletes 'top)
(define-key dired-mode-map [mouse-2] 'dired-find-file)
(define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode)

(use-package diff-hl)
(with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode))


(provide 'init-dired)
;;; init-dired.el ends here)