;;; package: -- configuration for IDE programming
;;; Commentary:
;;; Code:

;; settings for LSP MODE
;; need language-server，end env variable
;; rust--rust-analyzer
;; go--gopls
;; python--python-language-servicer
(use-package eglot
  :hook ((c-mode c++-mode go-mode js-mode python-mode rust-mode web-mode) . eglot-ensure)
  :bind (("C-c e f" . #'eglot-format)
         ("C-c e i" . #'eglot-code-action-organize-imports)
         ("C-c e q" . #'eglot-code-action-quickfix))
  :config
  ;; (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (defun eglot-actions-before-save()
    (add-hook 'before-save-hook (lambda ()
                                  (call-interactively #'eglot-format)
                                  (call-interactively #'eglot-code-action-organize-imports))))
  (add-to-list 'eglot-server-programs '(web-mode "vls"))
  (add-hook 'eglot--managed-mode-hook #'eglot-actions-before-save))

;; Golang
(use-package go-mode)

;; Lisp
;; (use-package paredit :init (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))
;; (use-package lispy :init (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

;; Python
(defmacro check-run-execute (exec-file &rest body)
  "Find the EXEC-FILE and run the BODY."

  `(if (not (executable-find ,exec-file))
       (message "[ERROR]: <%s> not found!" ,exec-file)
     ,@body))

;;;###autoload
(defun python-isort ()
  "Sort the imports with isort."
  (interactive)
  (check-run-execute "isort"
		     (shell-command-on-region
		      (point-min) (point-max)
		      "isort --atomic --profile=black -"
		      (current-buffer) t)))

;;;###autoload
(defun python-remove-all-unused-imports ()
  "Remove all the unused imports, do NOT use pyimport, as it has bugs.
eg.from datetime import datetime."
  (interactive)
  (check-run-execute "autoflake"
		     (shell-command
		      (format "autoflake -i --remove-all-unused-imports %s" (buffer-file-name)))
		     (revert-buffer t t t)))

(add-hook 'python-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook #'python-isort nil t)
	    (define-key python-mode-map (kbd "C-c p s") 'python-isort)
	    (define-key python-mode-map (kbd "C-c p r") 'python-remove-all-unused-imports)))

;; Rust
(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  (setq rustic-analyzer-command '("/home/evims/.cargo/bin/rust-analyzer"))
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

(use-package quickrun)


(provide 'init-eglot)

;;; init-eglot.el ends here
