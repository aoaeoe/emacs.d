;;; init-kbd.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(provide 'init-kbd)

;;; init-kbd.el ends here
