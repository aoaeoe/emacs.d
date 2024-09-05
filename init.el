;;; package --- init.el
;;; Commentary:
;;; Code:

;; Loaded target directory to load-path
(add-to-list 'load-path
	     (expand-file-name (concat user-emacs-directory "lisp")))

;; custom config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; os determine
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
;; you can determine Windows, too
;; (defconst *is-windows* (memq system-type '(ms-dos windows-nt cygwin)))

;; MacOS command key--->meta key
(when *is-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))
  
;; alias
(defalias 'yes-or-no-p 'y-or-n-p)

;; encoding config
(prefer-coding-system 'utf-8)
(unless *is-windows*
    (set-selection-coding-system 'utf-8))


;; init - UI
;; line-number config
(setq display-line-numbers-type 'relative)
(unless *is-windows*
  (global-display-line-numbers-mode t))

;; font-size
(set-face-attribute 'default nil :height 160)
;; indent 4
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(toggle-frame-maximized)

;; adjust the fonts
(defun available-font (font-list)
  "Get the first available font from FONT-LIST."
  (catch 'font
    (dolist (font font-list)
      (if (member font (font-family-list))
	  (throw 'font font)))))

;;;###autoload
(defun evims/setup-font ()
  "Font setup."

  (interactive)
  (let* ((efl '("Consolas" "DroidSansM Nerd Font" "FiraCode Nerd Font" "Courier New" "Monaco"))
	 (cfl '("楷体" "黑体" "STHeiti" "STKaiti"))
	 (cf (available-font cfl))
	 (ef (available-font efl)))
    (when ef
      (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
	(set-face-attribute face nil :family ef)))
    (when cf
      (dolist (charset '(kana han cjk-misc bopomofo))
	(set-fontset-font t charset cf))
      (setq face-font-rescale-alist
	    (mapcar (lambda (item) (cons item 1.2)) cfl)))))

;; settings for daemon mode
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (evims/setup-font))))
  (add-hook 'after-init-hook #'evims/setup-font))

;; Set the garbage collection threshold to speed up startup time.
(setq gc-cons-threshold most-positive-fixnum)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)


;; melpa
(setq package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(setq package-check-signature nil)

(require 'package)

;; init package
(unless (bound-and-true-p package-initialized)
  (package-initialize))

;; Refresh software source index
(unless package-archive-contents
  (package-refresh-contents))

;; package manager
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-always-demand nil
      use-package-expand-minimally t
      use-package-verbose t)

(require 'use-package)

;; startup time
(defun efs/display-startup-time ()
  "Statistic for the startup time."

  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(require 'init-package)
;; emacs29+ eglot
(require 'init-eglot)

;; if fcustom-file: load it
(when (file-exists-p custom-file)
  (load-file custom-file))


;;; init.el ends here
;; coding: utf-8
;; end:
