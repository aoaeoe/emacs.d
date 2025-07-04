;;; init-term.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

(use-package vterm
  :ensure t
  :commands vterm-mode
  :config
  (setq vterm-max-scrollback 50000)
  (setq vterm-kill-buffer-on-exit t)
  (defun leesin/vterm-mode-hook()
    ;;Don't prompt about dying processes when killing vterm
    (setq confirm-kill-processes nil)
    ;;Prevent premature horizontal scrolling
    (setq hscroll-margin 0)
    )
  :hook (vterm-mode . leesin/vterm-mode-hook)
  )

(use-package vterm-toggle
  :ensure t
  :commands (vterm-toggle vterm-toggle-cd)
  :config
  (setq vterm-toggle-popup-dedicated-frame nil)
  )

(provide 'init-term)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-term.el ends here
