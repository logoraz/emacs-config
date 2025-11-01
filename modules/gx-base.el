;;; gx-base.el --- Base Config/Defaults -*- lexical-binding: t -*-

;;; Commentary:
;;; Configuration of Emacs Core libraries.
;;; Default "Essential" Settings & Packages I use daily...
;;; See `comp.el' for review of Andrea Corallo's legendary world on native
;;; compilation (aka `eln' files).
;;; Research difference between emacs-next-tree-sitter & emacs-next-pgtk
;;; See https://www.emacswiki.org/emacs/PageBreaks
;;;  ‘forward-page’ (`C-x ]’ or `C-]’),
;;;  ‘backward-page’ (`C-x [’ or `C-[’), and `narrow-to-page' (‘C-x n p’).


;;; Code:



;;; File Settings: Auto Save, Backups, History, Bookmark, Recent Files,
;;; & Minibuffer control

;;; Auto Save: Prefix for generating auto-save-list-file-name
;; see - `auto-save-list-file-name'
(setq auto-save-list-file-prefix (expand-file-name "auto-save/.saves-"
                                                   gx-var-directory))
;; Backups
(setq  backup-directory-alist
       `(("." . ,(expand-file-name "backup" gx-var-directory)))
       make-backup-files t
       vc-make-backup-files nil
       backup-by-copying t
       version-control t
       delete-old-versions t
       kept-old-versions 6
       kept-new-versions 9
       delete-by-moving-to-trash t)

;;; History
(use-package savehist
  :diminish savehist-mode
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-file (expand-file-name "savehist.el" gx-var-directory))
  :config
  (setq history-length 500
        history-delete-duplicates t)
  (savehist-mode 1))

;; Bookmarks
(use-package bookmark
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" gx-var-directory)))

;;; Recent Files
(use-package recentf
  ;; TODO: Optimize use-package configuration for this!
  :diminish recentf-mode
  :init
  (setq recentf-save-file (expand-file-name "recentf" gx-var-directory)
        recentf-max-menu-items 50)
  ;; (customize-set-variable 'recentf-exlcude)
  :config
  (gx/ignore-messages
    (recentf-mode)))

;;; Minibuffer acrobatics
(defun gx/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(bind-key "C-c o" 'gx/switch-to-minibuffer)

;;; Info Files (Xtra)
(use-package info
  :init
  (gx/ensure-directory-exists "~/.cache/emacs/info")
  :config
  (add-to-list 'Info-directory-list
               (expand-file-name "info" user-emacs-directory))
  (setopt Info-default-directory-list Info-directory-list))



;;; External Modules

;;; Configure package PATH's
(use-package no-littering)


;;; UI Configuration
;;; Fonts Ligatures, Icons, Modeline, Themes, Tabs
;;;

(use-package ligature
  ;; Fonts & Theme Configuration
  ;; Fira Code & Ligature Support
  ;; See: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-ligature
  ;; See: https://github.com/mickeynp/ligature.el
  :diminish ligature-mode
  :config
  (defun gx/set-font-faces ()
    "Set font faces"
    (dolist
        (face
         '((default :font "Fira Code" :height 110)
           (fixed-pitch :font "Fira Code" :height 110)
           (variable-pitch :font "Iosevka Aile" :height 110)))
      (gx/set-face-attribute (car face) (cdr face))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (gx/set-font-faces))))
    (gx/set-font-faces))

  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://" ";;;" ";;;;" "!!!" "!!!!"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))


;; Load in local copy of nord theme - to develop and customize...
;; (add-to-list 'custom-theme-load-path (expand-file-name "~/.config/emacs/themes/"))
;; (load-theme 'kanagawa t)
;; https://github.com/tinted-theming/base16-emacs
(use-package all-the-icons)

(use-package nerd-icons
  :config
  ;; changes for newer version of nerd-icons
  ;; (add-to-list
  ;;  'nerd-icons-extension-icon-alist
  ;;  '("lisp" nerd-icons-mdicon "nf-md-yin_yang" :face nerd-icons-silver))

  ;; (add-to-list
  ;;  'nerd-icons-extension-icon-alist
  ;;  '("asd" nerd-icons-mdicon "nf-md-yin_yang" :face nerd-icons-silver))

  ;; (add-to-list
  ;;  'nerd-icons-mode-icon-alist
  ;;  '(lisp-mode nerd-icons-mdicon "nf-md-yin_yang" :face nerd-icons-silver))

  ;; Set "lisp" extensions/lisp-mode to Common Lisp Icon, instead of Scheme Icon...
  (add-to-list
   'nerd-icons-extension-icon-alist
   '("lisp" nerd-icons-sucicon "nf-custom-common_lisp" :face nerd-icons-silver))

  (add-to-list
   'nerd-icons-extension-icon-alist
   '("asd" nerd-icons-sucicon "nf-custom-common_lisp" :face nerd-icons-silver))

  (add-to-list
   'nerd-icons-mode-icon-alist
   '(lisp-mode nerd-icons-sucicon "nf-custom-common_lisp" :face nerd-icons-silver)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 32)
  (doom-modeline-buffer-encoding nil)
  ;; (doom-modeline-buffer-file-name-style 'file-name)
  :config
  (line-number-mode)
  (column-number-mode))

(use-package doom-themes
  :bind ("C-c d" . #'neotree)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; (load-theme 'doom-one :no-confirm)
  (load-theme 'doom-tomorrow-night :no-confirm)

  (defun gx/apply-theme (frame)
    "Apply my preferred theme to a new frame."
    (select-frame frame)
    (load-theme 'doom-tomorrow-night :no-confirm))

  ;; Needed to apply theme to new frames (and for emacs clients)
  (add-hook 'after-make-frame-functions
            'gx/apply-theme)

  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  (setq doom-themes-neotree-file-icons t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))



;;; Tabs (optional)
;;; minimal configuration for tab-bar --> see EXWM module
(use-package tab-bar
  :custom
  (tab-bar-show 1))



;;; Window Management Configuration

;; Window configuration presets
(defun gx/general-win-layout ()
  "Scaffold preferred general window layout."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (split-window-vertically))

(defun gx/calendar-win-layout ()
  "Scaffold org calendar window layout."
  (interactive)
  (calendar)
  (other-window 1)
  (split-window-horizontally))

;; Window layout persistence
;; #:TODO/250910 - Create this as a stack (alist/plist) to save multiple
;; window instances and pop to the desired layout based on workspace...
;; Also want to keep this current option available, i.e. saving any custom
;; window layout and restoraction on demand...
;; Instead of a stack, I can take the functional programming approach as saving
;; the layout as a closure --> see my functional calculator gcal for reference.
(defvar gx--current-window-layout nil
  "Persistant variable holding window layout")

(defun gx/save-current-windows ()
  "Save current window layout"
  (interactive)
  ;; #:TODO/250910 push current window configuration and workspace to stack
  (setq gx--current-window-layout (current-window-configuration)))

(defun gx/restore-last-windows ()
  "Restore window layout to last saved"
  (interactive)
  ;; #:TODO/250910 set based workspace
  (set-window-configuration gx--current-window-layout))



;;; Alternative Frame/Window Management & Notifications

(use-package beframe
  ;; Use beframe to handle desktops
  :diminish beframe-mode
  :bind (("C-c b" . beframe-transient))
  :custom
  (beframe-global-buffers '("*scratch*"
                            "*Messages*"
                            "*Backtrace*"
                            "*ednc-log*"
                            "*info*"
                            "*Ibuffer*"
                            "*Buffer List*"))
  :init (beframe-mode 1)
  :config
  (setq beframe-create-frame-scratch-buffer nil)
  (add-to-list 'display-buffer-alist
               '("*Buffer List*" . (display-buffer-same-window))))

(use-package ace-window
  :bind ("M-o" . 'ace-window))





(provide 'gx-base)
;;; gx-base.el ends here
