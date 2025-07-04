;;; lang-cc.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

(use-package cc-mode
  :ensure t
  :hook
  (cc-mode . (lambda () (setq tab-width 4)))
  :config
  (setq c-basic-offset 4) 
  )

(use-package cmake-mode
  :ensure t
  :hook
  (cmake-mode . (lambda () (setq tab-width 4)))
  )

;; Set clangd as the default C/C++ LSP
;;(setq lsp-clients-clangd-args '("-j=3"
;;                                "--background-index"
;;                                "--clang-tidy"
;;                                "--completion-style=detailed"
;;                                "--header-insertion=never"
;;                                "--header-insertion-decorators=0"))

(provide 'lang-cc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lang-cc.el ends here
