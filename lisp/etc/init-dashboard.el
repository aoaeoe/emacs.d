;;; init-dashboard.el ---   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code :

(use-package page-break-lines
  :ensure t
  :hook (after-init . global-page-break-lines-mode)
  :config
  (setq page-break-lines-char ?-)
  )

;; Load dashboard
;; (require 'dashboard)
;; (dashboard-setup-startup-hook)
(use-package dashboard
  :ensure t
  :hook (dashboard-mode . (lambda() (page-break-lines-mode 1)))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  (setq dashboard-startup-banner 'logo)
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts t)
  ;; To customize which widgets are dsiplayed and how many items are willing to show
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (agenda . 3)))
  ;; To show info about packages loaded and the init time.
  (setq dashboard-set-init-info t)
  ;; Set all-the-icons for icons
  (setq dashboard-icon-type 'all-the-icons)
  ;; To add icons to the widget headings and their items
  (setq dashboard-set-file-icons t
        dashboard-set-heading-icons t)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")
                                    (agenda . "calendar")))
  (setq dashboard-set-navigator t)
  (setq dashboard-banner-logo-title "EMACS")
  (when (display-graphic-p)
    (setq dashboard-navigator-buttons
          `(;; line1
            (
             (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
              "Github"
              "Browse Emacs Github"
              (lambda (&rest _) (browse-url "https://github.com/evims/.emacs.d")))
             (,(all-the-icons-octicon "history") "Recently" "Restore closed buffers"
              (lambda (&rest _) (progn (print "test") (recentf-open-files))))
             ("⚑" "Bookmarks" "Quick access bookmarks" (lambda (&rest _) (bookmark-set)))   
             )
            ))
    )
  (setq dashboard-set-separator t)
  (setq dashboard-page-separator "\n\f\n")

  )

(provide 'init-dashboard)

;;; init-dashboard.el ends here
