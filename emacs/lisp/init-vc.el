;;;; VCS
(use-package git-modes
  :ensure t
  :mode (("/\\.gitattributes\\'" . gitattributes-mode)
         ("/\\.git\\(config\\|modules\\)\\'" . gitconfig-mode)
         ("/\\.\\(git\\|docker\\|elpa\\)ignore\\'" . gitignore-mode)))

(use-package diff-hl)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'after-init-hook 'global-diff-hl-mode)

(with-eval-after-load 'diff-hl
    (define-key diff-hl-mode-map
        (kbd "<left-fringe> <mouse-1>")
        'diff-hl-diff-goto-hunk))

(provide 'init-vc)
;;; init-vc.el ends here