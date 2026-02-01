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
  :ensure nil
  :defer t
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
  :ensure nil
  :defer t
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" gx-var-directory)))

;;; Recent Files
(use-package recentf
  :ensure nil
  :defer t
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
  :ensure nil
  :defer t
  :init
  (make-directory (expand-file-name "info" gx-xdg-cache-home) t)
  :config
  (add-to-list 'Info-directory-list
               (expand-file-name "info" user-emacs-directory))
  (setopt Info-default-directory-list Info-directory-list))

;;; Enable Emacs server
(use-package server
  :ensure nil
  :hook (emacs-startup . gx/start-emacs-server)
  :config
  (defun gx/start-emacs-server ()
    "Hook function to start the Emacs Server."
    (interactive)
    ;; Set editor to use emacsclient
    (setenv "EDITOR" "emacsclient -c")
    (setenv "VISUAL" "emacsclient -c")

    (unless (server-running-p)
      (server-start))
    (message "Emacs Server started!!")))



;;; External Modules

;;; Configure package PATH's
(use-package no-littering
  :ensure t)


;;; UI Configuration
;;; Fonts Ligatures, Icons, Modeline, Themes, Tabs
;;;

(use-package ligature
  ;; Fonts & Theme Configuration
  ;; Fira Code & Ligature Support
  ;; See: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-ligature
  ;; See: https://github.com/mickeynp/ligature.el
  :ensure t
  :after server
  :diminish ligature-mode
  :config
  (defvar font-height
    (let ((height
           (pcase system-type
             ('windows-nt 90)
             ('gnu/linux  110)
             (_           110))))
      height)
    "Set the font height based on system-type.")
  (defun gx/set-font-faces ()
    "Set font faces"
    (dolist
        (face
         `((default :font "Fira Code" :height ,font-height)
           (fixed-pitch :font "Fira Code" :height ,font-height)
           (variable-pitch :font "Iosevka Aile" :height ,font-height)))
      (gx/set-face-attribute (car face) (cdr face))))

  (if (server-running-p)
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
(use-package all-the-icons
  :ensure t
  :defer t)

(use-package nerd-icons
  :ensure t
  :defer t
  :config
  ;; changes for newer version of nerd-icons
  (add-to-list
   'nerd-icons-extension-icon-alist
   '("lisp" nerd-icons-mdicon "nf-md-yin_yang" :face nerd-icons-silver))

  (add-to-list
   'nerd-icons-extension-icon-alist
   '("asd" nerd-icons-mdicon "nf-md-yin_yang" :face nerd-icons-silver))

  (add-to-list
   'nerd-icons-mode-icon-alist
   '(lisp-mode nerd-icons-mdicon "nf-md-yin_yang" :face nerd-icons-silver))

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
  :ensure t
  :defer t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 32)
  (doom-modeline-buffer-encoding nil)
  ;; (doom-modeline-buffer-file-name-style 'file-name)
  :config
  (line-number-mode)
  (column-number-mode))

(use-package doom-themes
  :ensure t
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
(use-package tab-bar
  :disabled ; Not currently using tab-bar
  :ensure t
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
  :ensure t
  :defer t
  :diminish beframe-mode
  :bind (("C-c b"   . beframe-transient)
         ("C-c f o" . make-frame-command)
         ("C-c f e" . delete-frame))
  :hook ((Buffer-menu-mode . gx/buffer-menu-colorize))
  :custom
  (beframe-global-buffers
   '("*scratch*" "*Messages*" "*Completions*" "*Backtrace*" "*info*"
     "*Buffer List*" "*Async-native-compile-log*"))
  :init (beframe-mode 1)
  :config
  (setq beframe-create-frame-scratch-buffer nil)

  (add-to-list 'display-buffer-alist
               '("*Buffer List*" . (display-buffer-same-window)))

(defvar gx--beframe-colors
    '("#5e81ac" "#81a1c1" "#88c0d0" "#8fbcbb")
    "List of colors for different beframes.")

(defvar gx--global-buffer-color "#b48ead"
  "Color for global buffers in buffer menu.")

(defvar gx--unassociated-buffer-color "#4c566a"
  "Color for buffers not associated with any frame.")

  (defun gx/beframe-buffer-color (buffer)
    "Return color for BUFFER based on its beframe association.
Returns specified color  for global buffers, frame-specific color otherwise."
    (when (bound-and-true-p beframe-mode)
      ;; Check if buffer is a global buffer
      (if (member (buffer-name buffer) beframe-global-buffers)
          gx--global-buffer-color
        ;; Otherwise find frame-specific color
        (let* ((frames (frame-list))
               (frame-index
                (cl-position-if
                 (lambda (frame)
                   (with-selected-frame frame
                     (memq buffer (beframe-buffer-list frame))))
                 frames)))
          (if frame-index
              (nth (mod frame-index (length gx--beframe-colors))
                   gx--beframe-colors)
            ;; Not associated with any frame
            gx--unassociated-buffer-color)))))

  (defun gx/buffer-menu-colorize ()
    "Colorize buffer menu entries by beframe."
    (when (eq major-mode 'Buffer-menu-mode)
      (save-excursion
        (goto-char (point-min))
        ;; (forward-line 2) ; Skip header lines
        (while (not (eobp))
          (when-let* ((buffer (tabulated-list-get-id))
                      (color (gx/beframe-buffer-color buffer)))
            (let ((inhibit-read-only t))
              (add-text-properties
               (line-beginning-position)
               (line-end-position)
               `(face (:foreground ,color :weight bold)))))
          (forward-line 1)))))

  ;; Add advice once globally, not per buffer
  (advice-add 'tabulated-list-print :after
              (lambda (&rest _)
                (when (eq major-mode 'Buffer-menu-mode)
                  (gx/buffer-menu-colorize)))))

(use-package ace-window
  :ensure t
  :defer t
  :bind ("M-o" . 'ace-window))





(provide 'gx-base)
;;; gx-base.el ends here
