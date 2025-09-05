;;; init.el --- Emacs initialization config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(unless (package-installed-p 'quelpa)
  (with-current-buffer
    (url-retrieve-synchronously
      "https://raw.githubusercontent.com/quelpa/master/quelpa.el"
      t t)
    (goto-char (point-max))
    (eval-print-last-sexp)))
(require 'quelpa)

(when (version< emacs-version "30")
  (error "This requires Emacs 30 and above!"))

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Speed up startup process,don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;;(defvar config-directory nil
;;  "This variable indicates where the config directory is.")

;;(setq config-directory (file-name-directory (or load-file-name buffer-file-name)))

;;(defvar package-user-dir nil
;;  "This variable indicates where the package directory is.")

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Load path
;; Optimize: Force `lisp' and `site-lisp' at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update 'load-path'."
  (dolist (dir '("site-lisp" "lisp/core" "lisp/etc" "lisp/lang"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(advice-add #'package-initialize :after #'update-load-path)

(update-load-path)

;; Indepentent custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-core)
(set-frame-font "FiraCode Nerd Font-11" t t)
(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
