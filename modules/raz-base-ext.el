;;;; raz-base-ext.el --- External Core RazEmacs, -*- lexical-binding: t; -*-

;;; Commentary:

;;; Author: Erik P. Almaraz

;;; References:
;;; Core features of my configuration provided from external
;;; packages, i.e. obtained via Guix or Melpa

;;; Code:

(defvar *ensure* t)

;;; Configure package PATH's
(use-package no-littering
  :ensure t)

(use-package ligature
  ;; Fonts & Theme Configuration
  ;; Fira Code & Ligature Support
  ;; See: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-ligature
  ;; See: https://github.com/mickeynp/ligature.el
  :ensure t
  :diminish ligature-mode
  :config
  (dolist
      (face
       '((default :font "Fira Code" :height 110)
         (fixed-pitch :font "Fira Code" :height 110)
         (variable-pitch :font "Iosevka Aile" :height 110)))
    (raz/set-face-attribute (car face) (cdr face)))
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

;;; Theme Configuration
;; Load in local copy of nord theme - to develop and customize...
;; (add-to-list 'custom-theme-load-path (expand-file-name "~/.config/emacs/themes/"))
;; (load-theme 'kanagawa t)
;; https://github.com/tinted-theming/base16-emacs
(use-package all-the-icons
  :ensure t)

(use-package nerd-icons
  :ensure t
  :config
  ;; Set "lisp" extensions/lisp-mode to Common Lisp Icon, instead of Scheme Icon...
  (add-to-list 'nerd-icons-extension-icon-alist
               '("lisp" nerd-icons-mdicon "nf-md-yin_yang" :face nerd-icons-silver))

  (add-to-list 'nerd-icons-extension-icon-alist
               '("asd" nerd-icons-mdicon "nf-md-yin_yang" :face nerd-icons-silver))

  (add-to-list 'nerd-icons-mode-icon-alist
               '(lisp-mode nerd-icons-mdicon "nf-md-yin_yang" :face nerd-icons-silver)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 28))

(use-package doom-themes
  :ensure t
  :bind ("C-c d" . #'neotree)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t      ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; (load-theme 'doom-one :no-confirm)
  (load-theme 'doom-tomorrow-night :no-confirm)

  (defun apply-my-theme (frame)
  "Apply my preferred theme to a new frame."
  (select-frame frame)
  (load-theme 'doom-tomorrow-night :no-confirm))

  (add-hook 'after-make-frame-functions
            'apply-my-theme)

  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  (setq doom-themes-neotree-file-icons t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package tab-bar
  :disabled
  :ensure t
  :after nord-theme
  :config
    ;; Set custome definte "Nord" faces for tab-bar
  (dolist
      (face
       '((tab-bar :foreground "#7b88a1" :background "#272C37")
         (tab-line :inherit tab-bar)
         (tab-bar-tab :inherit mode-line-highlight
                      :foreground "#b48ead"
                      :background "#272C37")
         (tab-bar-tab :box (:line-width 1 :color "#7b88a1" :style none))
         (tab-bar-tab-group-current :inherit tab-bar-tab)
         (tab-bar-tab-group-current :box (:line-width 1 :color "#3B4252" :style none))
         (tab-bar-tab-inactive :foreground "#7b88a1" :background "#272C37")
         (tab-bar-tab-inactive :box (:line-width 1 :color "#616e88" :style none))
         (tab-bar-tab-group-inactive :inherit tab-bar-tab-inactive)
         (tab-bar-tab-ungrouped :inherit tab-bar-tab-inactive)))
    (raz/set-face-attribute (car face) (cdr face))))


;;; Editing/IDE Package configurations
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name "undo-tree-hist/"
                               *raz-var-directory*))))
  :config
  (setq kill-do-not-save-duplicates t)
  (global-undo-tree-mode))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((eval-expression-minibuffer-setup
          lisp-interaction-mode
          emacs-lisp-mode
          lisp-mode
          scheme-mode) . enable-paredit-mode))

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :hook ((text-mode prog-mode) . ws-butler-mode))

(use-package magit
  :ensure t
  :defer 5
  :custom
  (magit-clone-always-transient nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (vc-follow-symlinks t))

;;; Workflow frame/tab workspaces
(use-package beframe
  :ensure t
  :diminish beframe-mode
  :bind-keymap ("C-c b" . beframe-prefix-map)
  :custom
  (beframe-global-buffers '("*Messages*" "*Backtrace*"))
  :init
  (beframe-mode t))

(use-package ace-window
  :ensure t
  :bind ("M-o" . 'ace-window))

(use-package flycheck
  :ensure t
  :diminish
  :init (global-flycheck-mode)
  :custom
  (flycheck-checker-error-threshold 2000 "Increase error threshold."))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(provide 'raz-base-ext)
;;; raz-base-ext.el ends here
