;;; lang-python.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

(use-package python-mode
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :hook
  (python-mode . (lambda () (setq tab-width 4)))
  )

(provide 'lang-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lang-python.el ends here
