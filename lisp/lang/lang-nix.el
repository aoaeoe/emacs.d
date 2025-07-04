;;; lang-nix.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-mode)
  :hook
  (nix-mode . (lambda () (setq tab-width 2)))
  )

(provide 'lang-nix)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lang-nix.el ends here
