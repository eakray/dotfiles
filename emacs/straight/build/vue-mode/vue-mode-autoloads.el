;;; vue-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from vue-mode.el

(autoload 'vue-mode-edit-all-indirect "vue-mode" "\
Open all subsections with `edit-indirect-mode' in seperate windows.
If KEEP-WINDOWS is set, do not delete other windows and keep the root window
open.

(fn &optional KEEP-WINDOWS)" t)
(autoload 'vue-mode "vue-mode" "\


(fn)" t)
(setq mmm-global-mode 'maybe)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(register-definition-prefixes "vue-mode" '("vue-"))

;;; End of scraped data

(provide 'vue-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; vue-mode-autoloads.el ends here
