;;; early-init.el ---
;;; Commentary:
;;; Code:

(unless (>= emacs-major-version 29)
  (error "EMACS v29+!"))

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 600000)))

(setq package-enable-at-startup nil)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(provide 'early-init)

;;; early-init.el ends here
;; Local Variables:
;; END
