;;; highlight-escape-sequences-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from highlight-escape-sequences.el

(autoload 'turn-on-hes-mode "highlight-escape-sequences" "\
Turn on highlighting of escape sequences." t)
(autoload 'turn-off-hes-mode "highlight-escape-sequences" "\
Turn off highlighting of escape sequences" t)
(defvar hes-mode nil "\
Non-nil if Hes mode is enabled.
See the `hes-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `hes-mode'.")
(custom-autoload 'hes-mode "highlight-escape-sequences" nil)
(autoload 'hes-mode "highlight-escape-sequences" "\
Toggle highlighting of escape sequences.

This is a global minor mode.  If called interactively, toggle the
`Hes mode' mode.  If the prefix argument is positive, enable the
mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='hes-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "highlight-escape-sequences" '("hes-"))

;;; End of scraped data

(provide 'highlight-escape-sequences-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; highlight-escape-sequences-autoloads.el ends here
