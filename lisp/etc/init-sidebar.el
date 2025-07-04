;;; init-sidebar.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

(use-package treemacs
  :ensure t
  :demand nil
  ;;:init
  ;;(with-eval-after-load 'winum
  ;;  (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-hidden-files               t
          treemacs-width                           25
          treemacs-width-is-initially-locked       nil
          ))
  (when sys/win32p
    (setq treemacs-python-executable "C:/Users/ASUS/AppData/Local/Programs/Python/Python310/python.exe"))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;;treemacs-perspective if you use perspective.el vs. persp-mode
(use-package treemacs-persp 
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;;treemacs-tab-bar if you use tab-bar-mode
(use-package treemacs-tab-bar 
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package minimap
  :defer t
  :ensure t
  :hook
  (after-init . (lambda()
                  (when (display-graphic-p)
                    (minimap-mode))))
  :config
  (setq minimap-window-location 'right
        minimap-update-delay 0.1
        minimap-width-fraction 0.05
        minimap-minimum-width 10)
  )

(provide 'init-sidebar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-sidebar.el ends here
