;;; init-evil.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

;; Set variable before evil due to evil-collection issue 60
(setq evil-want-integration t
      evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (require 'evil)
  :hook
  (after-init . evil-mode)
)

(use-package evil-collection
  :ensure t
  :after evil
  :hook
  (magit-mode . evil-collection-magit-init)
  (evil-mode . evil-collection-init)
  )

(provide 'init-evil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-evil.el ends here
