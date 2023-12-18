;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(use-package git-blamed)
(use-package git-modes)
(use-package git-timemachine)
(global-set-key (kbd "C-x v t") 'git-timemachine-toggle)

(use-package git-link)

(use-package magit
  :ensure t
  :after transient
  :commands (magit magit-status)
  :bind (("C-x g" . magit-status)))

(setq-default magit-diff-refine-hunk t)

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up))

(use-package magit-todos)

(use-package fullframe)
(with-eval-after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(use-package git-commit)
(add-hook 'git-commit-mode-hook 'goto-address-mode)

(setq magit-save-repository-buffers nil)

;;;###autoload
(defun my/magit-status ()
  "Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (locate-dominating-file buffer-file-name ".git"))))
         (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (call-interactively #'magit-status)
    (delete-other-windows)
    (when buffer-file-path
      (goto-char (point-min))
      (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                       (magit-section-show (magit-current-section))
                       (recenter)
                       t)
               do (condition-case nil
                      (magit-section-forward)
                    (error (cl-return (magit-status-goto-initial-section-1))))))))

;;;###autoload
(defun my/magit-save-buffer-show-status ()
  "Save buffer and show its changes in `magit-status'."
  (interactive)
  (save-buffer)
  (my/magit-status))

(provide 'init-git)
;;; init-git.el ends here
