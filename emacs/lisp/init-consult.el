(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
        ;;  ("C-c M-x" . consult-mode-command)
        ;;  ("C-c h" . consult-history)
        ;;  ("C-c k" . consult-kmacro)
        ;;  ("C-c m" . consult-man)
        ;;  ("C-c i" . consult-info)
        ;;  ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
        ;;  ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
        ;;  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
        ;;  ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
        ;;  ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
        ;;  ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
        ;;  ("M-#" . consult-register-load)
        ;;  ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
        ;;  ("C-M-#" . consult-register)
         ;; Other custom bindings
        ;;  ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
        ;;  ("M-g e" . consult-compile-error)
        ;;  ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
        ;;  ("M-g g" . consult-goto-line)             ;; orig. goto-line
        ;;  ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
        ;;  ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
        ;;  ("M-g m" . consult-mark)
        ;;  ("M-g k" . consult-global-mark)
        ;;  ("M-g i" . consult-imenu)
        ;;  ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
        ;;  ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
        ;;  ("M-s L" . consult-line-multi)
        ;;  ("M-s k" . consult-keep-lines)
        ;;  ("M-s u" . consult-focus-lines)
        ;;  ;; Isearch integration
        ;;  ("M-s e" . consult-isearch-history)
        ;;  :map isearch-mode-map
        ;;  ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
        ;;  ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
        ;;  ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
        ;;  ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
        ;;  ;; Minibuffer history
        ;;  :map minibuffer-local-map
        ;;  ("M-s" . consult-history)                 ;; orig. next-matching-history-element
        ;;  ("M-r" . consult-history))                ;; orig. previous-matching-history-element
        )
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
)

(use-package orderless
  :ensure t
  :init
  :config
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(provide 'init-consult)
;;; init-consult.el ends here