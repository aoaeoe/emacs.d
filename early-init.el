;;; early-init.el --- Emacs pre-initialization config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Enhance IO
(setq read-process-output-max ( * 1024 1024))

(setq process-adaptive-read-buffering nil)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
;; (setq gc-cons-threshold 134217728   ; 128mb
      gc-cons-percentage 1.0)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil  ;; obsolete since 29.1
      native-comp-jit-compilation nil)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Donnot load package from package cache
(setq package-quickstart nil)
;; `use-package' is builtin since 29.
;; It must be set before loading `use-package'.
(setq use-package-enable-imenu-support t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)

;; Prohibit resize windows first
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(setq inhibit-startup-message t)

(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

(set-fringe-mode 10)

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
