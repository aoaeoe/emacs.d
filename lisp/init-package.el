;;; package --- init-package
;;; Commentary:
;;; Code:

;; dashboard on startup
;; (use-package dashboard
;;  :defer nil
;;  :config
;;  (setq dashboard-center-content t
;;	dashboard-show-shortcuts t
;;	dashboard-startup-banner 'logo
;;	dashboard-items '((recents . 10)
;;			  (bookmarks . 5)
;;			  (projects . 5)))
;;  (dashboard-setup-startup-hook))

;; gruvbox-theme  you can use Built-in modus theme
(use-package gruvbox-theme
  :init (load-theme 'gruvbox-dark-soft t))
;; Automatically adjust to the most suitable theme style 
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package all-the-icons)
;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;; neotree, file tree manager
(use-package neotree
  :commands (neo-buffer--lock-width neo-buffer--unlock-width)
  :config (setq neo-autorefresh t
		neo-theme 'nerd
		neo-click-changes-root t
		neo-smart-open t
        neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind ("C-c C-t" . neotree-toggle))


;; shell env
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac nx x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("PATH" "GOPATH"))))

;; Quickly return to the beginning of the line. you can use C-a and M-m
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
	 ("C-c ^" . 'crux-top-join-line)
         ("C-x ," . 'crux-find-user-init-file)
         ("C-S-d" . 'crux-duplicate-current-line-or-region)
	 ("C-S-k" . 'crux-smart-kill-line)))

;; Delete empty lines
(use-package hungry-delete
  :bind (("C-c DEL" . hungry-delete-backward))
  :bind (("C-c d" . hungry-delete-forward)))

;; Move lines/blocks up and down
(use-package drag-stuff
  :bind (("<M-up>" . drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

;; ivy-counsel-swiper
(use-package ivy
  :defer 1
  :demand
  :hook (after-init . ivy-mode)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-initial-inputs-alist nil
	ivy-count-format "%d/%d "
	enable-recursive-minibuffers t
	ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (use-package ivy-posframe
    :when (display-grayscale-p)
    :init
    (setq ivy-posframe-display-functions-alist
	'((swiper . ivy-posframe-display-at-frame-center)
	  (complete-symbol . ivy-posframe-display-at-point)
	  (counsel-M-x . ivy-posframe-display-at-frame-center)
	  (counsel-find-file . ivy-posframe-display-at-frame-center)
	  (ivy-switch-buffer . ivy-posframe-dispaly-at-frame-center)
	  (t . ivy-posframe-display-at-frame-center)))
    (ivy-posframe-mode 1)))

(use-package counsel
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c f" . counsel-recentf)
	 ("C-c g" . counsel-git)))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper-isearch-backward))
  :config (setq swiper-action-recenter t
		swiper-include-line-number-in-search t))

;; Remember keyboard shortcuts
(use-package which-key
  :defer nil
  :config (which-key-mode))

;; auto complete
(use-package company
  :hook (after-init . global-company-mode)
  :config (setq company-minimum-prefix-length 1
                company-show-quick-access t))

;; restart emacs
(use-package restart-emacs)

;; flymake
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (global-set-key (kbd "M-n") #'flymake-goto-next-error)
  (global-set-key (kbd "M-p") #'flymake-goto-prev-error))

;; settings for yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config
  (add-to-list 'yas-snippet-dirs (concat
				 (file-name-directory user-emacs-directory)
				 "snippets"))
  (use-package yasnippet-snippets)
  (use-package auto-yasnippet
    :bind (("C-o" . aya-open-line)
	   ("H-w" . aya-create)
	   ("H-y" . aya-expand))))

;; settings for projectile
;; using after-init hook makes emacs starts up faster then config projectile-mode
(use-package projectile
  :hook (after-init . projectile-mode)
  ;; :config (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

;; Bracket pair alignment highlighting
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Quickly switch split screens
(use-package ace-window
  :bind (("M-o" . 'ace-window)))


(provide 'init-package)

;;; init-package.el ends here
