;;;; Programming Languages, Markup and Configurations
(use-package css-mode
  :mode "\\.css$"
  :custom
  (css-indent-offset 2))

(use-package js
  :mode (("\\.js\\'" . js-mode))
  :custom
  (js-indent-level 4))

(use-package vue-mode
  :config
  (setq mmm-submode-decoration-level 0))

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook css-mode)

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode))
  :hook ((markdown-mode . visual-line-mode)))

(use-package python
  :defer t
  :custom
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "-i --simple-prompt --pprint"))

(provide 'init-languages)
;;; init-languages.el ends here