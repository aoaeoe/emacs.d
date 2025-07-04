;;; init-complete.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

(use-package yasnippet
  :ensure t
  :defer t
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))

  ;;(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;; check corfu frame,if it exists,disable yassnippet keymap
  ;;:custom
  ;;(yas-keymap-disable-hook
  ;;(lambda () (and (frame-live-p corfu--frame)
  ;;                 (frame-visible-p corfu--frame))))
  )

;;Clone this repo somewhere local https://github.com/doomemacs/snippets
(use-package doom-snippets
  ;:load-path "~/.emacs.d/elpa/snippets"
  :load-path (lambda() (expand-file-name "snippets" package-user-dir))
  ;; :quelpa (doom-snippets
  ;;    :fetcher github
  ;;    :repo "doomemacs/snippets")
  :after yasnippet
  )

(use-package company
  :ensure t
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :hook ((after-init . global-company-mode))
  :bind
  (:map company-active-map
        ("<tab>" . company-select-next)
        ("<backtab>" . company-select-previous))
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line)
  :config
  (setq company-dabbrev-code-everywhere t
        company-dabbrev-code-modes t
        company-dabbrev-code-other-buffers 'all
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-other-buffers 'all
        company-require-match 'never
        company-minimum-prefix-length 2
        company-show-quick-access t
        ;; `company-show-numbers' is an obsolute variable,use `company-show-quick-access' instead
        company-tooltip-limit 10
        company-idle-delay 0
        company-echo-delay 0
        company-tooltip-offset-display 'scrollbar
        company-begin-commands '(self-insert-command)
        company-backends '((company-yasnippet company-dabbrev company-capf company-files)))
  ;;The code above can only show one type of complete method. 
  ;;Unless set all the company-backens in a group by add another ()

  ;;Use the code blow can show code-complete with yasnippet at the same time
  ;;(push '(company-semantic :with company-yasnippet) company-backends)
  ;;(push '(company-capf :with company-yasnippet) company-backends)
  ;;Refer to the doom emacs method,both can be shown at the same time.
  ;;See `init-lang.el'
  )

;;company-yasnippet disable after dot
(defun company-yasnippet/disable-after-dot (fun command &optional arg &rest _ignore)
(if (eq command 'prefix)
    (let ((prefix (funcall fun 'prefix)))
        (when (and prefix (not (eq (char-before (- (point) (length prefix)))
                            ?.)))
        prefix))
    (funcall fun command arg)))

(advice-add #'company-yasnippet :around #'company-yasnippet/disable-after-dot)

(provide 'init-complete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-complete.el ends here
