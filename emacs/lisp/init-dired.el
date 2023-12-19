;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'dired-mode-hook 'hl-line-mode)
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

(use-package consult-dir
  :ensure t
  :bind (("C-c C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-c C-d" . consult-dir)
         ("C-c C-j" . consult-dir-jump-file)))

(setq consult-dir-project-list-function #'consult-dir-projectile-dirs)

(provide 'init-dired)
;;; init-dired.el ends here)