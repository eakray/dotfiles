;;; init.el --- Initialization file. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Package management
;; Package management in Emacs can be done in several ways. I personally like
;; `use-package' together with package.el.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))
(setq package-quickstart t)

;; No need to activate all packages so early in either server or non-interactive
;; mode.
(when (or (daemonp) noninteractive)
  (package-initialize))

;; For the actual package configuration
(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (unless (ignore-errors (require 'use-package))
    ;; This is a seldomly-run part of my configuration, as `use-package' is
    ;; installed on Emacs' first run.
    (require 'package)
    (package-refresh-contents)
    (package-install 'use-package)
    ;; Only on the first run will all packages configured within this file be
    ;; ensured. This speeds up subsequent startups quite nicely.
    (setq use-package-always-ensure t)
    (require 'use-package)))

(setq use-package-always-ensure t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-exec-path)
(require 'bind-key)
(require 'init-editing-utils)
(require 'init-config)
(require 'init-themes)
(require 'init-consult)
(require 'init-minibuffer)
(require 'init-vc)
(require 'init-languages)
(require 'init-dired)
(require 'init-whichkey)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-projectile)
(require 'init-ibuffer)
(require 'init-corfu)
(require 'init-smart-mode-line)
(require 'init-git)

;;;; Backup
;; Silently deletes excess backup versions.
(setq delete-old-versions t)

;; Make numeric backup versions unconditionally.
(setq version-control t)

;; Make backup files even in version controlled directories.
(setq vc-make-backup-files t)

;; Keep all backups in one directory.
(let ((my-backup-dir (concat user-emacs-directory "backup/")))
  (setq backup-directory-alist
        `(("." . ,(file-name-as-directory my-backup-dir))))
  (unless (file-exists-p my-backup-dir)
    (make-directory my-backup-dir t)))

;;;; Auto-Saving
(let ((save-dir (concat user-emacs-directory "auto-save-list/")))
  (setq
   auto-save-file-name-transforms `((".*" ,(expand-file-name "\\2" save-dir) t))
   auto-save-list-file-name
   (concat save-dir (format ".saves-%d-%s~" (emacs-pid) (system-name))))
  (unless (file-exists-p save-dir) (make-directory save-dir t)))

;;;; History
(use-package savehist
  ;; Do not enable on daemon or batch mode.
  :if (and (not noninteractive) (not (daemonp)))
  :demand 2
  :custom
  ;; The default value is typically lower, but increasing it allows for a more
  ;; comprehensive history, which can be beneficial when needing to recall or
  ;; reuse previous inputs across sessions.
  (history-length 1000)
  ;; Enable the automatic deletion of duplicate entries from history.  By
  ;; default, this is disabled, but enabling it helps in keeping the history
  ;; clean and more manageable, especially when frequently reusing the same
  ;; inputs.
  (history-delete-duplicates t)
  :init
  ;; Save minibuffer history.  Utilizing `savehist-mode' within the :init
  ;; section is preferable over setting the analogous variable in :custom due to
  ;; performance reasons.  It ensures that the mode is activated before the
  ;; package is fully loaded, which can lead to a quicker startup.
  (savehist-mode t))

(use-package recentf
  :if (not noninteractive)
  :defer 2
  :custom
  (recentf-max-saved-items 100)
  (recentf-keep '(recentf-keep-default-predicate
		  file-remote-p file-readable-p))
  :config
  (recentf-mode)
  (when (fboundp 'recentf-save-list)
    (run-with-idle-timer (* 3 60) t #'recentf-save-list)))

;; I use C source to understand and debug built-in functions.
(let ((src "~/src/emacs.git"))
  (when (or (file-directory-p src)(file-symlink-p src))
    (setq source-directory (expand-file-name (substitute-in-file-name src)))))

;;;; Emacs Server
(declare-function server-running-p "server")
(add-hook 'after-init-hook
          #'(lambda ()
              (require 'server)
              (unless (server-running-p)(server-start))))

;;;; Project management
(use-package project
  :defer t
  :commands (project-find-file
             project-switch-to-buffer
             project-switch-project
             project-switch-project-open-file)
  :bind (:map project-prefix-map
              ("p" . my-project-switch-project)
              ("s" . my-switch-project-and-kill-buffers)
              ("R" . project-remember-projects-under)
              ("K" . project-kill-buffers))
  :custom
  (project-vc-ignores '("#*#" ".#*" "*~" ".*~" "*.*~" "*.elc" "*.pyc" "*.o"
   "*.lo" "*.la" "*.sock" "*.zwc" ".DS_Store" "__pycache__" "node_modules"))
  :config
  ;; Auto clean up zombie projects from `project-list-file'
  (run-at-time "07:00pm" (* 24 60 60) 'project-forget-zombie-projects)
  ;; Use ripgrep if installed
  (when (shell-command-to-string "command rg --version")
    (setq xref-search-program 'ripgrep))

  (defun my-switch-project-and-kill-buffers ()
    "Kill all buffers of the current project, then switch to a new project."
    (interactive)
    (project-kill-buffers t)
    (call-interactively 'project-switch-project))

  (declare-function project-prompt-project-dir "project")
  (defun my-project-switch-project (dir)
    "\"Switch\" to another project by running an Emacs command.
Directly use `project-find-file' instead of getting prompted.

When called in a program, it will use the project corresponding
to directory DIR."
    (interactive (list (project-prompt-project-dir)))
    (let ((project-current-directory-override dir))
      (project-find-file))))

;;; init.el ends here
