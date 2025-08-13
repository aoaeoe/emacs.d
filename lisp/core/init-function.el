;;; init-function.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

(require 'cl-lib)
;; Self Custom
(let ((self-custom-path (expand-file-name "self-custom.el" user-emacs-directory)))
  (when (file-exists-p self-custom-path)
    (load-file self-custom-path)))

(defun evims/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'evims/display-startup-time)

(defun evims/childframe-workable-p ()
  "Whether childframe is workable."
  (not (or noninteractive
          emacs-basic-display
          (not (display-graphic-p)))))

(defun evims/childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (and (eq evims-completion-style 'childframe)
      (evims/childframe-workable-p)))

(defvar evims/proxy-enabled nil)
;;;###autoload
(defun evims/toggle-proxy ()
  (interactive)
  (if evims/proxy-enabled
      (progn
        (setq url-proxy-services nil)
        (setq evims/proxy-enabled nil)
        (message "代理已关闭."))
    (progn
      (when sys/win32p
        (setq url-proxy-services
              '(("http" . "127.0.0.1:10809")
                ("https" . "127.0.0.1:10809"))))
      (when sys/linux
        (setq url-proxy-services
              '(("http" . "127.0.0.1:20171")
                ("https" . "127.0.0.1:20171"))))
      (setq evims/proxy-enabled t)
      (message "代理已开启."))))

;;;###autoload
(defun animate-text (text)
  ;; https://github.com/matrixj/405647498.github.com/blob/gh-pages/src/emacs/emacs-fun.org
  (interactive "stext: ")  ; s means read-string
  (switch-to-buffer (get-buffer-create "*butterfly*"))
  (erase-buffer)
  (animate-string text 10))

;;;###autoload
(defun evims/indent-region (numSpaces)
  "该函数会根据当前行或选区的起始和终止位置，将这些行的文本进行缩进处理。"
  (let ((regionStart nil)
        (regionEnd nil)
        (start nil)
        (end nil))
    (progn
      ; 默认是当前行的起始位置和终止位置
      (setq regionStart (line-beginning-position))
      (setq regionEnd (line-end-position))

      ; 如果有选区，则使用选区的起始位置和终止位置
      (when (use-region-p)
        (setq regionStart (region-beginning))
        (setq regionEnd (region-end)))

      (save-excursion
        ;; 通过`narrow-to-region'和`widen'修复缩进影响下一行
        ;; 限制缩进范围
        (narrow-to-region regionStart regionEnd)

        ; 恢复位置
        (goto-char regionStart)                ; 移动到选区的起始位置
        (setq start (line-beginning-position)) ; 保存行的起始位置
        (goto-char regionEnd)                  ; 移动到选区的终止位置
        (setq end (line-end-position))         ; 保存行的终止位置

        ; 对 start 和 end 之间的文本进行缩进
        (indent-rigidly start end numSpaces)
        (setq deactivate-mark nil) ; 恢复选区
        ;; 修复缩进范围
        (widen)
        )
      )
    )
  )

;;;###autoload
(defun evims/untab-region ()
  "命令函数，它的作用是将选定的文本块反向缩进。"
  (interactive)
  ;; (evims/indent-region -4)
  ;; `bolp' 仅适用于判断单个点，而不适用于选区
  (if indent-tabs-mode
      (if (use-region-p)
          (evims/unindent-region-with-tabs)
        (call-interactively #'backward-delete-char))
    (evims/unindent-region-without-tabs)
    )
  )

;;;###autoload
(defun evims/unindent-region-without-tabs ()
  ;; 如果存在选区，则调用 `evims/indent-region' 函数，并计算缩进值传递给它。
  ;; 如果没有选区，则计算需要反向缩进多少，并使用 `delete-char'
  (if (use-region-p)
      (evims/indent-region (- 0 tab-width))
      ;; (progn
      ;;   (goto-char (region-beginning))
      ;;   (let* ((start (region-beginning))
      ;;          (offset (current-indentation))
      ;;          (movement (% (- start offset) tab-width))
      ;;          (spaces (- (if (= 0 movement) tab-width
      ;;                       (- tab-width movement)))))
      ;;     (evims/indent-region spaces)))
    (unless (bolp)
      (save-excursion
        (when (> (current-column) (current-indentation))
          (back-to-indentation)) 
        (let* ((movement (% (current-column) tab-width))
               (spaces (if (= 0 (current-column)) 0
                         (- (if (= 0 movement) tab-width
                            (- tab-width movement))))))
          (delete-char spaces))
        )
      )
    )
  )

;;;###autoload
(defun evims/unindent-region-with-tabs ()
  "在选定区域内逐行删除起始字符(认为它们是\t)。"
  (let* ((regionStart (region-beginning))
         (regionEnd (region-end)))
    (save-excursion
      (save-restriction
        (narrow-to-region regionStart regionEnd)
        (goto-char regionStart)
        (while (< (point) regionEnd)
          (beginning-of-line)
          ;;(call-interactively #'backward-delete-char)
          (delete-char 1)
          (forward-line 1))
        (setq deactivate-mark nil) ; 恢复选区
        )
      (widen)
      )
    )
  )

;;;###autoload
(defun evims/tab-region ()
  "命令函数，它用于将选定的文本缩进。"
  (interactive)
  (if indent-tabs-mode
      (progn
        (if (use-region-p)
            (evims/indent-region-with-tabs) 
          (insert "\t"))) 
    (evims/indent-region-without-tabs)
    )
  )

;;;###autoload
(defun evims/indent-region-without-tabs ()
  ;; 如果存在选区，调用 `evims/indent-region' 函数，并计算缩进值
  (let ((offset nil)
        (movement nil)
        (spaces nil))
    ;; 如果没有选区，则计算需要缩进的空格数，使用 `insert'
    (if (use-region-p)
        (evims/indent-region tab-width)
        ;; (progn
        ;;   (goto-char (region-beginning))
        ;;   (setq offset (current-indentation)
        ;;         movement (% offset tab-width)
        ;;         spaces (if (= 0 movement) tab-width
        ;;                (- tab-width movement)))
        ;;   (evims/indent-region spaces)
        ;;   )
      (progn
        (setq movement (% (current-column) tab-width)
              spaces (if (= 0 movement) tab-width
                     (- tab-width movement)))
       (insert (make-string spaces ? )))
      )
    )
  )

;;;###autoload
(defun evims/indent-region-with-tabs ()
  "为选定区域逐行添加 \t"
  (let* ((regionStart (region-beginning))
         (regionEnd (region-end)))
    (save-excursion
      (narrow-to-region regionStart regionEnd)
      (goto-char regionStart)
      (while (< (point) regionEnd)
        (beginning-of-line)
        (insert "\t")
        (forward-line 1))
      (setq deactivate-mark nil)  ; 恢复选区
      (widen)
      )
    )
  )

;;;###autoload
(defun evims/hack-tab-key ()
  "命令函数，它重新定义了 <tab> 和 <backtab> 键的行为。"
  ;; 通过调用 local-set-key 函数，它将 <tab> 键绑定到 evims/tab-region 函数，将 <backtab> 键绑定到 evims/untab-region 函数。
  (interactive)
  (local-set-key (kbd "<tab>") 'evims/tab-region)
  (local-set-key (kbd "<backtab>") 'evims/untab-region))

(add-hook 'prog-mode-hook 'evims/hack-tab-key)

(defun evims-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (style-path (concat user-emacs-directory "config/org-style/gongzhitaao-style.css"))
           (final-path (if (file-exists-p style-path) style-path nil)))
      (when final-path
        (setq-local org-html-head-include-default-style nil)
        (setq-local org-html-head (concat
                                   "<style type=\"text/css\">\n"
                                   "<!--/*--><![CDATA[/*><!--*/\n"
                                   (with-temp-buffer
                                     (insert-file-contents final-path)
                                     (buffer-string))
                                   "/*]]>*/-->\n"
                                   "</style>\n"))))))

(add-hook 'org-export-before-processing-hook 'evims-org-inline-css-hook)

(provide 'init-function)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-function.el ends here
