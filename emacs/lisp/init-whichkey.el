(use-package which-key)
(which-key-mode)
;; same as default, except all keys from local maps shown first
(setq which-key-sort-order 'which-key-local-then-key-order)
(setq which-key-idle-delay .5)
(provide 'init-whichkey)
;;; init-whichkey.el ends here