;;;; Sane defaults
(use-package emacs
  :custom
  ;; Use tab key as completion option.
  (tab-always-indent 'complete)
  ;; No scratch message.
  (initial-scratch-message "")
  ;; Disable start-up screen.
  (inhibit-startup-screen t)
  ;; Configure the Scratch Buffer's Mode.
  (initial-major-mode 'text-mode)
  ;; Save custom variables in custom.el.
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  ;; Disable lockfiles on Windows, as they are a hassle.
  (create-lockfiles (not (member system-type '(windows-nt))))
  ;; No tabs - except for specific files, which Emacs can identify.
  (indent-tabs-mode nil)
  ;; Redefine line and column format. It will looks like " 278:59 ".
  (mode-line-position '((line-number-mode ("%l" (column-number-mode ":%c")))))
  :hook
  ((text-mode prog-mode) . (lambda () (setq show-trailing-whitespace t)))
  :init
  ;; Show column number next to line number in mode line.
  (column-number-mode t)
  (when (or (file-exists-p custom-file) (file-symlink-p custom-file))
    (load custom-file t t))
    ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  ;;;; Undo
  ;; Don't group undo steps. Why?
  ;; .. without this it groups actions into a fixed number of steps which feels unpredictable.
  (fset 'undo-auto-amalgamate 'ignore)
  ;; Increase undo limits. Why?
  ;; .. ability to go far back in history can be useful, modern systems have sufficient memory.
  ;; Limit of 64mb.
  (setq undo-limit 6710886400)
  ;; Strong limit of 1.5x (96mb)
  (setq undo-strong-limit 100663296)
  ;; Outer limit of 10x (960mb).
  ;; Note that the default is x100), but this seems too high.
  (setq undo-outer-limit 1006632960))

;; Save point position between sessions
(use-package saveplace
  ;; Do not enable on daemon or batch mode.
  :if (and (not noninteractive) (not (daemonp)))
  :demand 2
  :init
  ;; Automatically save place in each file.  Utilizing `save-place-mode' within
  ;; the :init section is preferable over setting the analogous variable in
  ;; :custom due to performance reasons.  It ensures that the mode is activated
  ;; before the package is fully loaded, which can lead to a quicker startup.
  (save-place-mode t))

(setq mac-command-modifier 'meta)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((code-dir "~/code")
                  (home-dir "~/"))
              (if (file-exists-p code-dir)
                  (dired code-dir)
                (dired home-dir)))))

(defun my/terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

(use-package diminish)

;; Just blink the modeline on errors.
(setq ring-bell-function #'my/terminal-visible-bell)

;; Highlight matching parentheses when the point is on them.
(add-hook 'after-init-hook #'show-paren-mode)

;; Highlight brackets according to their depth.
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; I prefer not to have any of the GUI elements.  This keeps the window clean
;; and speeds up loading a bit.
(setq-default use-file-dialog nil)
(setq-default use-dialog-box nil)

;; Show line numbers
(use-package display-line-numbers
  :custom (display-line-numbers-width 4)
  :hook (prog-mode . display-line-numbers-mode))

(use-package elec-pair
  :hook
  (after-init . electric-pair-mode)
  (minibuffer-setup . (lambda () (electric-pair-local-mode 0))))

;; Delete trailing whitespace on save in all modes.  If there are modes or
;; projects where trailing whitespace should be retained, it is suggested to
;; override this setting on a per-mode or per-project basis by removing
;; `delete-trailing-whitespace' from `before-save-hook' locally or by using
;; directory-local variables to change the behavior as needed.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Ensure a final newline on save in all modes.  If there are modes or projects
;; where a final newline should not be added, it is suggested to override this
;; setting on a per-mode or per-project basis by removing the local setting for
;; `require-final-newline' or by using directory-local variables to change the
;; behavior as needed.
(setq-default require-final-newline t)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

(setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq-default indicate-buffer-boundaries 'left)

;; Set default tab width to 4 spaces
(setq-default tab-width 4)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Specific settings for JavaScript
(setq js-indent-level 4)

(provide 'init-config)
;;; init-config.el ends here