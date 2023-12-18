(use-package lsp-mode)
;;(add-hook 'XXX-mode-hook #'lsp)

(use-package lsp-ui)
(lsp-ui-mode)

(add-hook 'go-mode-hook #'lsp)
(add-hook 'eslint-mode-hook #'lsp)

(provide 'init-lsp)
;;; init-lsp.el ends here