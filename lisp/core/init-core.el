;;; init-core.el ---   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Const variable
(require 'init-const)
(require 'init-custom)
;; Self function
(require 'init-function)

(require 'init-package)

;; Edit Mode
(require 'init-editor)
;; Search engine
(require 'init-vertico)

(require 'init-ui)
(require 'init-dashboard)
(require 'init-modeline)

;;(defun load-language-part-file()
;; Complete Frame
(require 'init-complete)
;; Language support
(require 'init-lang)
;;)

(defun load-function-part-file()
  ;; Sidebar
  (require 'init-sidebar)
  ;; Term
  (require 'init-term)
)

;; delay load
(defun load-other-part-file()
  ;; Org Mode
  (require 'init-org)
  ;; Input Method
  (require 'init-input)
  ;; Key bindings
  (require 'init-kbd)
)

(add-hook 'after-init-hook #'load-function-part-file)
(add-hook 'after-init-hook #'load-other-part-file)

(provide 'init-core)
;;; init-core.el ends here
