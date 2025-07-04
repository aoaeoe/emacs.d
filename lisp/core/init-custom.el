;;; init-custom.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :
(require 'init-const)

;; Coding System UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Default variables
(setq inhibit-splash-screen t)
(setq delete-by-mving-to-trash nil)
;; Disable Autosave
(setq auto-save-default nil)
;; Disable Backup
(setq make-backup-files nil)
(setq backup-inhibited t)

;; use short answers for YES/NO ect.
(setq use-short-answers t)
(setq-default tab-width 4
              indent-tabs-mode nil)

;; Default Mode
(setq-default major-mode 'text-mode)

;; Srcoll smoothly
(setq scroll-step 1
      scroll-conservatively 10000)

(when sys/win32p
  (let ((default-dir "E:/"))
    (when (file-exists-p default-dir)
      (setq-default default-directory default-dir))))

(dolist (hook (list
              'org-mode-hook
              'emacs-lisp-mode-hook
              'conf-mode-hook
              'c-mode-hook
              'c++-mode-hook
              'java-mode-hook
              'python-mode-hook))
(add-hook hook #'(lambda ()
               ;; 设置自动换行
               (setq truncate-lines nil)
               ;; 针对中文折行的问题进行设置
               ;;(setq word-wrap nil)
               (auto-fill-mode -1)
               )))
;;(setq word-wrap-by-category t)

;; Simplify confirm process
(defalias 'yes-or-no-p 'y-or-n-p)

(defcustom leesin-lsp-format-on-save nil
  "Auto format buffers on save." )

(defcustom leesin-lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode)
  "Auto format buffers ignored on save." )

(defcustom leesin-completion-style 'minibuffer
  "Completion display style.")

(setq leesin-lsp-format-on-save t)
(setq leesin-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode markdown-mode) )

(provide 'init-custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
