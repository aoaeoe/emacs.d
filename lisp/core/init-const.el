;;; init-const.el ---   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Judge Operation System
(defconst sys/win32p
   (memq system-type '(cygwin windows-nt ms-dos))
   "We are running on a WinNT system.")

(defconst sys/linux
   (eq system-type 'gnu/linux)
   "We are running on a GNU/Linux system.")

(defconst sys/mac
   (eq system-type 'darwin)
   "We are running on a MacOS system.")

(defconst emacs/>=30p
  (>= emacs-major-version 30)
  "Emacs is 30 or above.")

(provide 'init-const)
;;; init-const.el ends here
