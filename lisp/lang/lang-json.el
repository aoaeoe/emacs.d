;;; lang-json.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :hook
  (json-mode . (lambda () (setq tab-width 4)))
  )

(provide 'lang-json)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lang-json.el ends here
