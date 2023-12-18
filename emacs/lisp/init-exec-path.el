;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell)

(exec-path-from-shell-copy-env "PATH")
(provide 'init-exec-path)
;;; init-exec-path.el ends here
