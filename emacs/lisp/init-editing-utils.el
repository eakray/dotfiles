(defun my/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'my/kill-back-to-indentation)

(defun my/insert-line-before ()
  "Inserts a newline(s) above the line containing the cursor."
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (newline)))

(global-set-key (kbd "C-S-u") 'my/insert-line-before)

;; newline-without-break-of-line
(defun my/insert-line-after ()
  "1. move to end of the line.
  2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "C-S-o") 'my/insert-line-after)

;; ;;; Some basic preferences
;; (setq-default
;;  cursor-type '(bar . 2)
;;  blink-cursor-interval 0.4
;;  bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
;;  buffers-menu-max-size 30
;;  case-fold-search t
;;  column-number-mode t
;;  ediff-split-window-function 'split-window-horizontally
;;  ediff-window-setup-function 'ediff-setup-windows-plain
;;  indent-tabs-mode nil
;;  create-lockfiles nil
;;  auto-save-default nil
;;  make-backup-files nil
;;  mouse-yank-at-point t
;;  save-interprogram-paste-before-kill t
;;  scroll-preserve-screen-position 'always
;;  set-mark-command-repeat-pop t
;;  tooltip-delay 1.5
;;  truncate-lines nil
;;  truncate-partial-width-windows nil)

;;; Some basic preferences
(setq-default
;;  cursor-type '(bar . 2)
 blink-cursor-interval 0.4)

(use-package move-dup)
(global-set-key [M-up] 'move-dup-move-lines-up)
(global-set-key [M-down] 'move-dup-move-lines-down)
(global-set-key [M-S-up] 'move-dup-move-lines-up)
(global-set-key [M-S-down] 'move-dup-move-lines-down)

(global-set-key (kbd "C-c d") 'move-dup-duplicate-down)
(global-set-key (kbd "C-c u") 'move-dup-duplicate-up)

(use-package highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)

;; Huge files

(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))

(use-package vlf)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))

;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
(defun my/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'my/newline-at-end-of-line)

(use-package emacs
  :ensure nil
  :defer 1
  :config
  (setq-default cursor-type 'bar)
  ;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
  ;; Using `advice' here to make it easy to reverse in custom
  ;; configurations with `(advice-remove 'yes-or-no-p #'y-or-n-p)'
  ;;
  ;; N.B. Emacs 28 has a variable for using short answers, which should
  ;; be preferred if using that version or higher.
  (if (boundp 'use-short-answers)
      (setq use-short-answers t)
    (advice-add 'yes-or-no-p :override #'y-or-n-p)))

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
