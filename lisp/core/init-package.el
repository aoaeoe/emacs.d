;;; init-package.el ---   -*- lexical-binding: t; -*-
;; -*- no-byte-compile: t; -*-
;;; Commentary:

;;; Code :

;; Initialize package sources
(require 'package)
;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; if install quelpa, use proxy or ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
(setq package-archives '(;;("melpa" . "https:///melpa.org/packages/")
			                   ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ;;("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ))

(package-initialize)

;(unless package-archive-contents
;  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; set `use-package' variables
(setq use-package-always-ensure t
      use-package-always-defer nil
      use-package-always-demand nil
      use-package-expand-minimally t
      use-package-verbose t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

(use-package quelpa
  :ensure t
  :defer t
  :config  
  (setq quelpa-upgrade-p nil
        quelpa-update-melpa-p nil)
  )

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(use-package package-utils
  :ensure t
  :commands
  (package-utils-upgrade-all
   package-utils-list-upgrades
   package-utils-upgrade-by-name
   package-utils-remove-by-name
   package-utils-upgrade-all-and-quit
   package-utils-upgrade-all-and-restart
   package-utils-upgrade-all-and-recompile)
  )

;; Auto update packages
(unless (fboundp 'package-upgrade-all)
  (use-package auto-package-update
    :defer t
    :init
    (setq auto-package-update-delete-old-versions t
            auto-package-update-hide-results t)
    (defalias 'upgrade-packages #'auto-package-update-now)))

(provide 'init-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
