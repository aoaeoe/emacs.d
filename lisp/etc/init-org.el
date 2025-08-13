;;; init-org.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :
(require 'init-const)
;; (require 'init-function) ;; 按照加载顺序此时已经加载了

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :custom
  ;; 设置标题行折叠符号
  (org-ellipsis " ▾")
  ;; TODO标签美化
  (org-fontify-todo-headline t)
  ;; DONE标签美化
  (org-fontify-done-headline t)
  ;; 引用块美化
  (org-fontify-quote-and-verse-blocks t)
  ;; 隐藏宏标记
  (org-hide-macro-markers t)
  :config
  (setq evil-auto-indent nil)
  (org-toggle-inline-images)
  (setq org-hide-leading-stars nil
        org-hide-emphasis-markers t
        org-startup-indented t
        org-modern-timestamp nil)
  (defun evims/self-org-mode-hook()
    ;;(interactive)
    (org-indent-mode t)
    (variable-pitch-mode -1)
    (auto-fill-mode -1)
    (visual-line-mode t)
    )
  (defun evims/self-org-prettify-symbols()
    ;;(interactive)
    (setq prettify-symbols-alist
          (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                  '(
                    ("[ ]"              . 9744)         ; ☐
                    ("[X]"              . 9745)         ; ☑
                    ("[-]"              . 8863)         ; ⊟
                    ("#+begin_src"      . "✑")         ; ✎
                    ;;("#+end_src"        . 9633)         ; □
                    ("#+end_src"        . 8863)         ; ⊟
                    ("#+begin_example"  . 129083)       ; 🠻
                    ("#+end_example"    . 129081)       ; 🠹
                    ("#+results:"       . 9776)         ; ☰
                    ("#+attr_latex:"    . "🄛")
                    ("#+attr_html:"     . "🄗")
                    ("#+attr_org:"      . "🄞")
                    ("#+name:"          . "🄝")         ; 127261
                    ("#+caption:"       . "🄒")         ; 127250
                    ("#+date:"          . "📅")         ; 128197
                    ;;("#+author:"        . "💁")         ; 128100
                    ("#+author:"        . "👤")         ; ✎
                    ("#+setupfile:"     . 128221)       ; 📝
                    ("#+email:"         . 128231)       ; 📧
                    ("#+startup:"       . 10034)        ; ✲
                    ("#+options:"       . 9965)         ; ⛭
                    ("#+title:"         . 10162)        ; ➲
                    ("#+subtitle:"      . 11146)        ; ⮊
                    ("#+downloaded:"    . 8650)         ; ⇊
                    ("#+language:"      . 128441)       ; 🖹
                    ("#+begin_quote"    . 171)          ; «
                    ("#+end_quote"      . 187)          ; »
                    ("#+begin_results"  . 8943)         ; ⋯
                    ("#+end_results"    . 8943)         ; ⋯
                    )))
    (setq prettify-symbols-unprettify-at-point t)
    (prettify-symbols-mode 1)
    )
  :hook
  (org-mode . evims/self-org-prettify-symbols)
  (org-mode . evims/self-org-mode-hook)
  ;; Get the org symbols set
  :custom-face
  ;; 设置Org mode标题以及每级标题行的大小
  ;;(org-block-begin-line ((t (:underline t :background unspecified))))
  ;;(org-block-end-line ((t (:overline t :underline nil :background unspecified))))
  (org-document-title ((t (:height 1.4 :weight bold))))
  (org-level-1 ((t (:height 1.2 :weight bold))))
  (org-level-2 ((t (:height 1.15 :weight bold))))
  (org-level-3 ((t (:height 1.1 :weight bold))))
  (org-level-4 ((t (:height 1.05 :weight bold))))
  (org-level-5 ((t (:height 1.0 :weight bold))))
  (org-level-6 ((t (:height 1.0 :weight bold))))
  (org-level-7 ((t (:height 1.0 :weight bold))))
  (org-level-8 ((t (:height 1.0 :weight bold))))
  (org-level-9 ((t (:height 1.0 :weight bold))))
  )

(use-package org-modern
  :ensure t
  :hook
  ((org-mode . (lambda ()
                 (setq org-modern-hide-stars 'leading)
                 (global-org-modern-mode t)))
   (org-agenda-finalize . org-modern-agenda))
  :config
  ;; 禁用 table 修改，原因是不好看
  (setq org-modern-table nil)
  ;; 标题行型号字符
  (setq org-modern-star ["◉" "○" "✸" "✳" "◈" "◇" "✿" "❀" "✜"])
  ;; 额外的行间距，0.1表示10%，1表示1px
  (setq-default line-spacing 0.1)
  ;; tag边框宽度，还可以设置为 `auto' 即自动计算
  (setq org-modern-label-border 1)
  ;; 复选框美化
  (setq org-modern-checkbox
        '((?X . #("▢✓" 0 2 (composition ((2)))))
          (?- . #("▢–" 0 2 (composition ((2)))))
          (?\s . #("▢" 0 1 (composition ((1)))))))
  ;; 列表符号美化
  (setq org-modern-list
        '((?- . "•")
          (?+ . "◦")
          (?* . "▹")))
  ;; 代码块左边加上一条竖边线（需要 `org-startup-indented' 关闭方有效
  ;; 如果启用了 `visual-fill-column-mode' 会很难看）
  (setq org-modern-block-fringe nil)
  ;; 代码块类型美化，我们使用了 `prettify-symbols-mode'
  (setq org-modern-block-name nil)
  ;; #+关键字美化，我们使用了 `prettify-symbols-mode'
  (setq org-modern-keyword nil)
  )

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-inside-latex t)
  )

(add-hook 'org-mode-hook (lambda ()
                            (setcar (nthcdr 0 org-emphasis-regexp-components)
                                    "-[:multibyte:][:space:]('\"{")
                            (setcar (nthcdr 1 org-emphasis-regexp-components)
                                    "-[:multibyte:][:space:].,:!?;'\")}\\[")
                            (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
                            (org-element-update-syntax)
                            ;; 规定上下标必须加 {}，否则中文使用下划线时它会以为是两个连着的下标
                            (setq org-use-sub-superscripts "{}")))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  ;;(add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  ;;(evil-org-set-key-theme)
  ;; org-at-heading-or-items-p
  ;;(evil-define-key 'insert 'evil-org-mode
  ;;  (kbd "TAB") 'org-metaright
  ;;  (kbd "<backtab>") 'org-metaleft)
  (evil-define-key 'normal 'evil-org-mode
    (kbd "O") 'evil-open-above)
  (evil-define-key 'visual 'evil-org-mode
    (kbd "<tab>") 'evims/tab-region)
  (evil-define-key 'visual 'evil-org-mode
    (kbd "<backtab>") 'evims/untab-region)
  )

;; Standardize the tabel width in different fonts
(use-package valign
  :ensure t
  ;;:config
  ;;(setq valign-fancy-bar 1
  ;;      valign-signal-parse-error 1)
  :hook (org-mode . valign-mode)
  )

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  ;;:pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

;; Set margin for org-present
(defun evims/org-mode-visual-fill()
  (setq visual-fill-column-width 80
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun evims/org-mode-visual-fill-quit()
  ;;(setq visual-fill-column-width 0
  ;;      visual-fill-column-center-text nil)
  (visual-fill-column-mode 0))

(use-package visual-fill-column
  :ensure t
  :hook ((org-present-mode . evims/org-mode-visual-fill)
         (org-present-mode-quit . evims/org-mode-visual-fill-quit)))

;; Org-present Configuration
(defun evims/org-present-prepare-slide()
  (org-overview)
  ;;`org-show-entry' is an obsolete function; use `org-fold-show-entry' instead.
  (org-fold-show-entry)
  ;;`org-show-children' is an obsolete function; use `org-fold-show-children' instead.
  (org-fold-show-children))

(defun evims/org-present-hook()
  (display-line-numbers-mode nil)
  (setq-local face-remapping-alist '((header-line (:height 4.0) variable-pitch)))
  (setq text-scale-mode-amount 1)
  (text-scale-mode 1)
  (setq header-line-format " ")
  (org-display-inline-images)
  (evims/org-present-prepare-slide))

(defun evims/org-present-quit-hook()
  (display-line-numbers-mode t)
  (text-scale-mode 0)
  (setq header-line-format nil)
  (org-remove-inline-images))

(defun evims/org-present-prev()
  (interactive)
  (org-present-prev)
  (evims/org-present-prepare-slide))

(defun evims/org-present-next()
  (interactive)
  (org-present-next)
  (evims/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-j" . evims/org-present-next)
         ("C-c C-k" . evims/org-present-prev))
  :hook ((org-present-mode . evims/org-present-hook)
         (org-present-mode-quit . evims/org-present-quit-hook)))

(when sys/linux
  (let ((tasks-file "~/Documents/org/tasks.org"))
    (when (file-exists-p tasks-file)
      (setq org-agenda-files `(,tasks-file)))))
(when sys/win32p
  (let ((tasks-file "E:\\org\\task.org"))
    (when (file-exists-p tasks-file)
      (setq org-agenda-files `(,tasks-file)))))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
