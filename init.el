;;; init.el --- Initialization File -*- lexical-binding: t -*-

;;; Commentary:
;;;
;;; gxEMACS framework/initializations
;;; |--> gnu/linux & windows-nt


;;; Code:



;;; gxemacs System-Wide Variables & Environment Establishment
;;;
;;; user-emacs-directory --> ~/.cache/emacs/ (early-init)

(defgroup gxemacs nil
  "Grafted-X Emacs (gxEmacs) Configuration."
  :tag "gxEMACS"
  :link '(url-link "")
  :group 'emacs)

(defcustom gx-var-directory
  (expand-file-name "var" user-emacs-directory)
  "Default var directory."
  :type 'string
  :group 'gxemacs)

(defcustom gx-etc-directory
  (expand-file-name "etc" user-emacs-directory)
  "Default etc directory."
  :type 'string
  :group 'gxemacs)

(defcustom gx-modules-directory (expand-file-name "modules" gx-xdg-config-home)
  "Default Emacs Modules directory."
  :type 'string
  :group 'gxemacs)

(defcustom gx-load-custom-file nil
  "When non-nil, load `custome.el' after user's config file, `config.el'."
  :type 'string
  :group 'gxemacs)

;; Create .cache directories to avoid prompts for their creation during initial
;; Emacs installation:
(make-directory gx-etc-directory t)
(make-directory gx-var-directory t)


;; Add the modules directory to the load path
(add-to-list 'load-path gx-modules-directory)

;; Set custom file to NOT be our init file.
(gx/setopts custom-file (expand-file-name "custom.el" gx-etc-directory)
            "Set preferred location of custom-file")

(when gx-load-custom-file
  (load custom-file t :no-error :no-message))



;;; Configure use-package

;; Enable `use-package' statistics - must be set before any `use-package' forms.
;; Run command M-x `use-package-report' to see
;; 1. How many packages were loaded,
;; 2. What stage of initialization they've reached,
;; 3. How much aggregate time they've spend (roughly).
(gx/use-modules use-package)

(gx/setopts use-package-compute-statistics t "Enable use-package statistics."
            use-package-catch-errors t "Catch errors during package installation.")



;;; Load Config Modules
(gx/use-modules gx-base
                gx-completions
                gx-dired
                gx-vcs
                gx-clide
                gx-org)




(provide 'init)
;;; init.el ends here
