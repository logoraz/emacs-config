;;; init.el --- Initialization File -*- lexical-binding: t -*-

;;; Commentary:
;;;
;;; gEXWM framework/initializations


;;; Code:
(require 'server)


;;; gexwm System-Wide Variables & Environment Establishment
;;;
;;; user-emacs-directory --> ~/.cache/emacs/ (early-init)

(defgroup gxemacs nil
  "GuixOS Emacs (aka gEMACS) Configuration."
  :tag "gxEMACS"
  :link '(url-link "")
  :group 'emacs)

;; Set GUIX packages to Emacs Load Path
(defcustom gx-emacs-packages-path
  (expand-file-name "~/.guix-home/profile/share/emacs/site-lisp/")
  "Guix Home Profile Emacs Packages PATH."
  :type 'string
  :group 'gexwm)

(defcustom gx-var-directory
  (expand-file-name "var" user-emacs-directory)
  "Default var directory."
  :type 'string
  :group 'gexwm)

(defcustom gx-etc-directory
  (expand-file-name "etc" user-emacs-directory)
  "Default etc directory."
  :type 'string
  :group 'gexwm)

(defcustom gx-modules-directory (expand-file-name "modules" gx-xdg-config-home)
  "Default Emacs Modules directory."
  :type 'string
  :group 'gexwm)

(defcustom gx-load-custom-file nil
  "When non-nil, load `custome.el' after user's config file, `config.el'."
  :type 'string
  :group 'gexwm)

;; construction
(defvar gx-guix? (not (file-directory-p (expand-file-name "/gnu/store/")))
  "Flag variable, true if were not using GNU Guix etc.")

(defvar gx-exwm? (string-equal (getenv "XDG_CURRENT_DESKTOP")
                               "exwm")
  "Flag variable, true if an EXWM session and nil otherwise.")
;; construction

;; Guix Related Ennvironment Setup
(when (eq system-type 'gnu/linux)
  (load (concat gx-emacs-packages-path "subdirs.el") :no-error :no-message)
  (add-to-list 'load-path gx-emacs-packages-path))

(when (eq system-type 'gnu/linux)
  (guix-emacs-autoload-packages))

;; Add the modules directory to the load path
(add-to-list 'load-path gx-modules-directory)

;; Set custom file to NOT be our init file.
(gx/setopts custom-file (expand-file-name "custom.el" gx-etc-directory)
            "Set preferred location of custom-file")

(when gx-load-custom-file
  (load custom-file t :no-error :no-message))


(if (eq system-type 'windows-nt)
    (set-frame-parameter nil 'undecorated nil))

;; set in early-init...
;; (gx/setopts inhibit-startup-echo-area-message user-login-name
;;             "Disable startup echo area message per user")



;;; Configure use-package

;; Enable `use-package' statistics - must be set before any `use-package' forms.
;; Run command M-x `use-package-report' to see
;; 1. How many packages were loaded,
;; 2. What stage of initialization they've reached,
;; 3. How much aggregate time they've spend (roughly).
(gx/setopts use-package-compute-statistics t "Enable use-package statistics.")



;;; Load Config Modules
(gx/setopts debug-on-error t "Set debugging on error as default!")
;; disable for now as it greatly impacts startup!
;; (package-initialize)

;;; EXWM
(gx->defhook gx/exwm--start-hookfn
  "Start EXWM after Emacs Initialization & lazarus-hookfn.
Should be loaded after gx/lazarus--hookfn --> `early-init'."

  (;;function body
   (when gx-exwm?
     (require 'gx-exwm-init)
     (require 'gx-exwm-conf)))

  :disable? t
  :hook window-setup-hook
  :depth 90)

;; Foundation Modules
(require 'gx-base)
(require 'gx-completions)
(require 'gx-dired)
(require 'gx-ide)

;; Office/Mail/Chat
(require 'gx-org)
(require 'gx-mail)
(require 'gx-desktop)

;; Start Emacs Server Daemon if not already started
(unless (daemonp)
  (server-start))





(provide 'init)
;;; init.el ends here
