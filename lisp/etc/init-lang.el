;; Language Server
(use-package eglot
  :bind ("C-c e f" . eglot-format)
  :init
  (advice-add 'eglot-code-action-organize-imports :before #'eglot-format-buffer)
  (add-hook 'eglot-managed-mode-hook (lambda () (add-hook 'before-save-hook #'eglot-format-buffer)))
  (add-hook 'prog-mode-hook
      (lambda () (unless (member major-mode '(emacs-lisp-mode))
       (eglot-ensure)))))

;; ;;(use-package treesit
;; (when (and (fboundp 'treesit-available-p) (treesit-available-p))
;;   :mode (("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode)
;;          ("\\.go\\'" . go-ts-mode)
;;          ("/go\\.mod\\'" . go-mod-ts-mode)
;;          ("\\.rs\\'" . rust-ts-mode)
;;          ("\\.ts\\'" . typescript-ts-mode)
;;          ("\\.y[a]?ml\\'" . yaml-ts-mode))
;;   :config (setq treesit-font-lock-level 4)
;;   ;; go-ts-mode indent 4 space
;;   (add-hook 'go-ts-mode-hook
;;             (lambda ()
;;               (setq go-ts-mode-indent-offset 4) ; indent width 4
;;               (setq tab-width 4)                ; tab width 4
;;               (setq indent-tabs-mode nil)))     ; indent not with tab
;;   :init
;;   (setq major-mode-remap-alist
;;   '((sh-mode         . bash-ts-mode)
;;     (c-mode          . c-ts-mode)
;;     (c++-mode        . c++-ts-mode)
;;     (c-or-c++-mode   . c-or-c++-ts-mode)
;;     (css-mode        . css-ts-mode)
;;     (js-mode         . js-ts-mode)
;;     (js-json-mode    . json-ts-mode)
;;     (makefile-mode   . cmake-ts-mode)
;;     (python-mode     . python-ts-mode)
;;     (ruby-mode       . ruby-ts-mode)
;;     (conf-toml-mode  . toml-ts-mode)))
;;   (setq treesit-language-source-alist
;;   '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
;;     (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
;;     (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
;;     (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
;;     (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
;;     (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
;;     (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
;;     (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
;;     (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
;;     (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
;;     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
;;     (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
;;     (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
;;     (make       . ("https://github.com/alemuller/tree-sitter-make"))
;;     (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
;;     (org        . ("https://github.com/milisims/tree-sitter-org"))
;;     (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
;;     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
;;     (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
;;     (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
;;     (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
;;     (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
;;     (vue        . ("https://github.com/merico-dev/tree-sitter-vue"))
;;     (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
;;     (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
;;     (zig        . ("https://github.com/GrayJack/tree-sitter-zig")))))

(when (and (fboundp 'treesit-available-p) (treesit-available-p))

  ;; 设置更高的语法高亮级别
  (setq treesit-font-lock-level 4)

  ;; 重定向传统 major-mode 到对应的 tree-sitter 模式
  (setq major-mode-remap-alist
        '((sh-mode         . bash-ts-mode)
          (c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (c-or-c++-mode   . c-or-c++-ts-mode)
          (css-mode        . css-ts-mode)
          (js-mode         . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (makefile-mode   . cmake-ts-mode)
          (python-mode     . python-ts-mode)
          (ruby-mode       . ruby-ts-mode)
          (conf-toml-mode  . toml-ts-mode)))

  ;; 指定源码仓库，用于 M-x treesit-install-language-grammar
  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make       . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (org        . ("https://github.com/milisims/tree-sitter-org"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
          (vue        . ("https://github.com/merico-dev/tree-sitter-vue"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (zig        . ("https://github.com/GrayJack/tree-sitter-zig"))))

  ;; 自动匹配文件名启用对应 tree-sitter 模式
  (add-to-list 'auto-mode-alist '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

  ;; Go 缩进设置（不使用 tab，宽度 4）
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (setq go-ts-mode-indent-offset 4)
              (setq tab-width 4)
              (setq indent-tabs-mode nil))))


(provide 'init-lang)

;;; init.el ends here
;;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
