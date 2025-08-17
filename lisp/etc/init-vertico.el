;;; init-vertico.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

;; Enable vertico
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :bind
  (:map vertico-map
        ("<tab>" . vertico-insert)    ; Choose selected candidate
        ("<escape>" . vertico-exit) ; Close minibuffer
        ("DEL" . 'vertico-directory-delete-char)
        ;;("C-M-n" . vertico-next-group)
        ;;("C-M-p" . vertico-previous-group)
        )
  ;;(map! :map vertico-map "DEl" #'vertico-directory-delete-char)
  :custom
  (vertico-count 15)  ; Number of candidates to display
  (vertico-resize nil)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle t)
  ;; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode t)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  :custom-face
  ;; hightlight the current option
  (vertico-current ((t (:background "#4a3f5a"))))
  )

;; 检查变量，支持childframe则使用vertico-posframe
(when (evims/childframe-completion-workable-p)
  (use-package vertico-posframe
    :ensure t
    :hook (vertico-mode . vertico-posframe-mode)
    :init (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center
                vertico-posframe-parameters
                '((left-fringe . 8)
                  (right-fringe . 8)))))

;; A few more useful configurations
(use-package emacs
  :ensure t
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  )

(use-package pinyin-isearch
  :ensure t)

;; Fuzzy find orders
(use-package orderless
  :ensure t
  :init
  ;; & is for company because space will break completion
  (setq completion-styles '(orderless partial-completion basic)
        orderless-component-separator "[ &]" 
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  :config
  ;; make completion support pinyin
  ;; refer to https://emacs-china.org/t/vertico/17913/2
  ;;(defun completion-regex-pinyin (str)
  ;;  (orderless-regexp (pinyinlib-build-regexp-string str)))
  ;;(add-to-list 'orderless-matching-styles 'completion-regex-pinyin)
  )

;; Display the purpose and comment of each command in minibuffer
(use-package marginalia
  :ensure t
  :hook
  (after-init . marginalia-mode)
  :config
  (marginalia-mode t)
  )

(use-package consult
  :ensure t
  :after
  (vertico)
  :bind 
  (([remap goto-line]                     . consult-goto-line)
   ([remap isearch-forward]               . consult-line-symbol-at-point)
   ; my-consult-ripgrep-or-line
   ([remap switch-to-buffer]              . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
   ([remap yank-pop]                      . consult-yank-pop)
   ([remap apropos]                       . consult-apropos)
   ([remap bookmark-jump]                 . consult-bookmark)
   ([remap goto-line]                     . consult-goto-line)
   ([remap imenu]                         . consult-imenu)
   ([remap multi-occur]                   . consult-multi-occur)
   ([remap recentf-open-files]            . consult-recent-file)
   ("C-x j"                               . consult-mark)
   ("C-c g"                               . consult-ripgrep)
   ("C-c f"                               . consult-find)
   ;;("\e\ef"                               . consult-locate)
   ; need to enable locate first
   ("C-c n h"                             . consult-find-org-headings)
   ;;:map org-mode-map
   ;;("C-c C-j"                             . consult-org-heading)
   :map minibuffer-local-map
   ("C-r"                                 . consult-history)
   :map isearch-mode-map
   ("C-;"                                 . consult-line)
   :map prog-mode-map
   ("C-c C-j"                             . consult-outline)
   )
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-window)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; search all org file headings under a directory, see:
  ;; https://emacs-china.org/t/org-files-heading-entry/20830/4
  (defun consult-find-org-headings (&optional match)
    "find headngs in all org files."
    (interactive)
    (consult-org-heading match (directory-files org-directory t "^[0-9]\\{8\\}.+\\.org$")))

  ;; Use `consult-ripgrep' instead of `consult-line' in large buffers
  (defun consult-line-symbol-at-point ()
    "Consult line the synbol where the point is"
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  )

(use-package color-rg
  :quelpa (color-rg
     :fetcher github
     :repo "manateelazycat/color-rg")
  :ensure t
  :after vertico
  :config
  (when sys/win32p
    (setq color-rg-search-ignore-rules nil))
)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :config
  (setq histroy-length 25)
  (savehist-mode 1))

(provide 'init-vertico)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vertico.el ends here
