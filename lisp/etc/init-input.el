;;; init-input.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :
(require 'init-const)

(when sys/linux
  (use-package fcitx
    :ensure t
    :defer 3
    :config
    (setq fcitx-use-dbus nil
          fcitx-remote-command "fcitx5-remote")
    (fcitx-aggressive-setup))
)

(use-package pangu-spacing
  :ensure t
  :defer 3
  ;:hook (text-mode . pangu-spacing-mode)
  :config
  (global-pangu-spacing-mode 1)
  ;; Always insert `real' space in org-mode.
  (add-hook 'org-mode-hook
            #'(lambda ()
               (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))))

(provide 'init-input)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-input.el ends here
