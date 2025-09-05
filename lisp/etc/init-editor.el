;;; init-editor.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

(defvar evims/indent-tabs-modes-list
  '(makefile-gmake-mode
    makefile-mode
    )
  )

(defun evims/toggle-indent-tabs-mode ()
  (if (member major-mode evims/indent-tabs-modes-list)
      (setq indent-tabs-mode t)
    (setq indent-tabs-mode nil)))

(add-hook 'after-change-major-mode-hook 'evims/toggle-indent-tabs-mode)

(use-package highlight-indent-guides
  :quelpa (highlight-indent-guides
     :fetcher github
     :repo "DarthFennec/highlight-indent-guides")
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?|)
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-odd-face "darkgray")
  (set-face-foreground 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
)

(use-package beacon
  :ensure t
  :hook
  (after-init . beacon-mode)
  )

;; Show line number
;;(setq display-line-numbers-type 't)
;;(setq display-line-numbers 'absolute)
(use-package display-line-numbers
  :ensure t
  :hook ((prog-mode yaml-mode conf-mode org-mode) . display-line-numbers-mode)
  ;;:config
  ;;(global-display-line-numbers-mode t)
  ;; line number align right
  :init
  (setq display-line-numbers-width-start t)
  )

(use-package newcomment
  :ensure nil
  :bind ([remap comment-dwim] . #'comment-or-uncomment)
  :config
  (defun comment-or-uncomment ()
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
          (call-interactively 'comment-dwim)
        (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  :custom
  (comment-auto-fill-only-comments t))

(use-package htmlize
  :ensure t
  :defer t)

(provide 'init-editor)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-editor.el ends here
