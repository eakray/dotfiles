(use-package vertico
  :demand t
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :ensure t
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
        :map minibuffer-local-map
        ("C-M-b" . my/minibuffer-backward-kill))
  :init
  (vertico-mode)
  (vertico-indexed-mode)
  (vertico-reverse-mode)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

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

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))


(use-package consult-flycheck)

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;;(use-package orderless
;;  :ensure t
;;  :custom
;;  (completion-styles '(orderless basic))
;;  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark-consult)


(use-package consult-flycheck)

(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
(global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
(global-set-key [remap goto-line] 'consult-goto-line)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here