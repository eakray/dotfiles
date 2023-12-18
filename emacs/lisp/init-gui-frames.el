;; Suppress GUI features

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;; Change global font size easily

(use-package default-text-scale)
(add-hook 'after-init-hook 'default-text-scale-mode)

(when window-system
  (set-frame-position (selected-frame) 180 0)
  (set-frame-size (selected-frame) 150 47))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here