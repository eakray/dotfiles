;;; fullframe-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from fullframe.el

(autoload 'fullframe/current-buffer-window-config "fullframe" "\
Return the window-configuration stored for the current buffer.")
(autoload 'fullframe/erase-current-buffer-window-config "fullframe" "\
Forget the window config associated with the current buffer.")
(autoload 'fullframe/set-current-buffer-window-config "fullframe" "\
Associate the current buffer with the window-configuration WCONF.

(fn WCONF)")
(autoload 'fullframe/split-screen "fullframe" "\
After COMMAND-ON is executed and only one window present in
  the current frame, split the frame in two windows ('below or
  'right, depending on DIRECTION being `horizontal' or
  `vertical') and switch the new window to the buffer
  SECOND-BUFFER (name or symbol). If SWITCH-TO-SECOND-BUFFER is
  not `nil', the window holding SECOND-BUFFER will be activated.

(fn COMMAND-ON COMMAND-OFF SECOND-BUFFER &optional DIRECTION SWITCH-TO-SECOND-BUFFER SIZE)" nil t)
(autoload 'fullframe "fullframe" "\
Save window/frame state when executing COMMAND-ON.

Advises COMMAND-ON so that the buffer it displays will appear in
a full-frame window.  The previous window configuration will be
restored when COMMAND-OFF is executed in that buffer.  If
KILL-ON-COFF is non-nil, then the buffer will also be killed
after COMMAND-OFF has completed.

This function uses `defadvice' on versions of emacs < 24.4,
`advice-add' otherwise.

AFTER-COMMAND-ON-FUNC is called after COMMAND-ON was called and
the window it generated is the only one in in the frame.

(fn COMMAND-ON COMMAND-OFF &optional KILL-ON-COFF AFTER-COMMAND-ON-FUNC)" nil t)
(register-definition-prefixes "fullframe" '("fullframe/"))

;;; End of scraped data

(provide 'fullframe-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; fullframe-autoloads.el ends here
