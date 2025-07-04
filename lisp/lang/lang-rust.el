;;; lang-rust.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :hook
  (rust-mode . (lambda () (setq tab-width 4)))
  )

(provide 'lang-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lang-rust.el ends here
