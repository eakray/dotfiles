;; -*- lexical-binding: t; -*-

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(require 'init-exec-path)
(use-package diminish)
(use-package scratch)
(use-package command-log-mode)

(require 'init-config)
(require 'init-dired)
(require 'init-themes)
(require 'init-whichkey)
(require 'init-minibuffer)
(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-projectile)
(require 'init-editing-utils)
(require 'init-recentf)
(require 'init-ibuffer)
(require 'init-lsp)
(require 'init-corfu)
(require 'init-smart-mode-line)
(require 'init-window)

(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'lsp)
  (setq mmm-submode-decoration-level 0))

(use-package magit)

(use-package go-mode)

(pixel-scroll-precision-mode 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-bright))
 '(warning-suppress-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;(use-package super-save
 ;;  :defer 1
;;   :diminish super-save-mode
;;   :config
;;  (super-save-mode +1)
;;  (setq super-save-auto-save-when-idle t))

;; Revert Dired and other buffers
;;(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
;;(global-auto-revert-mode 1)

;; (use-package savehist
 ;;  :config
;;   (setq history-length 25)
;;   (savehist-mode 1))

  ;; Individual history elements can be configured separately
  ;;(put 'minibuffer-history 'history-length 25)
  ;;(put 'evil-ex-history 'history-length 50)
  ;;(put 'kill-ring 'history-length 25))
