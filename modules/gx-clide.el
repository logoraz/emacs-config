;;; gx-clide.el --- CL IDE -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:



;;; General Editing/Dev Tools

;; Interpreter Mode Alist
;; (add-to-list 'interpreter-mode-alist '("Lisp" . lisp-mode))

;; .dir-local variables for development projects
(gx/setopts enable-local-eval t
            enable-local-variables :safe
            "Set the safe variables, and ignore the rest.")

(set-default-coding-systems 'utf-8)

(gx/setopts global-auto-revert-non-file-buffers t
            tab-width 8
            indent-tabs-mode nil
            ;; "Use spaces instead of tabs."
            sentence-end-double-space t
            large-file-warning-threshold 100000000
            find-file-visit-truename t)

(global-auto-revert-mode 1)
(delete-selection-mode)

(use-package display-line-numbers
  :hook ((scheme-mode lisp-mode emacs-lisp-mode)
         . display-line-numbers-mode))

(use-package display-fill-column-indicator
  ;; TODO: Customize theme color for this element -> via ':config' keyword
  :diminish
  ;; Only activate for lisp-mode
  :hook ((prog-mode org-mode) . display-fill-column-indicator-mode)
  :custom
  (fill-column 81)
  (display-fill-column-indicator-column fill-column)
  :config
  ;; Make fill-column-indicator face darker --> line-number face
  ;; theme value #5c5e5e --> #3f4040 (good with doom-tomorrow-night theme)
  (gx/set-face-attribute 'fill-column-indicator '(:foreground "#3f4040")))

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  ;; Save & Restore Window configuration
  ;; https://www.emacswiki.org/emacs/EdiffMode
  (add-hook
   'ediff-load-hook
   (lambda ()
     (add-hook 'ediff-before-setup-hook
               (lambda ()
                 (setq ediff-saved-window-configuration
                       (current-window-configuration))))
     (let ((restore-window-configuration
            (lambda ()
              (set-window-configuration ediff-saved-window-configuration))))
       (add-hook 'ediff-quit-hook
                 restore-window-configuration
                 'append)
       (add-hook 'ediff-suspend-hook
                 restore-window-configuration
                 'append)))))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((eval-expression-minibuffer-setup
          lisp-interaction-mode
          emacs-lisp-mode
          lisp-mode
          scheme-mode)
         . enable-paredit-mode))

(use-package undo-tree
  :ensure (undo-tree :pin gnu)
  :diminish undo-tree-mode
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name "undo-tree-hist/"
                               gx-var-directory))))
  :config
  (setq kill-do-not-save-duplicates t)
  (global-undo-tree-mode))

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :hook ((text-mode prog-mode) . ws-butler-mode))

(use-package flycheck
  :ensure t
  :diminish
  :init (global-flycheck-mode)
  :custom
  (flycheck-global-modes '(emacs-lisp-mode scheme-mode))
  (flycheck-checker-error-threshold 2000 "Increase error threshold."))

(use-package colorful-mode
  :ensure t
  :diminish
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

(use-package xr
  ;; converts regex strings to rx sexp syntax
  ;; https://github.com/mattiase/xr
  ;; :ensure (xr :pin melpa)
  :ensure t)

(use-package project
  :disabled)



;;; Common Lisp IDE

(use-package lisp-comment-dwim
  :disabled ; experimental --> creates an unwanted project file in user directory
  :vc (:url "https://github.com/dotemacs/lisp-comment-dwim.el" :branch "main")
  :config
  (lisp-comment-dwim-setup-keybindings))

(use-package sly
  :ensure t
  ;; Enable sly IDE for Common Lisp
  :hook ((lisp-mode . sly-editing-mode)
         (lisp-mode . gx/sly-auto-connect))
  :custom
  (inferior-lisp-program (executable-find "sbcl")
                         "Set default lisp to Steel Bank Common Lisp.")
  :config
  ;; Disable Sylvester the cat
  (setq sly-mrepl-pop-sylvester nil)

  ;; Provide proper syntax highlighting for `defsystem'
  (font-lock-add-keywords
   'lisp-mode
   '(("(\\s-*\\(defsystem\\)\\>" 1 font-lock-keyword-face prepend)))

  ;; Invoke SLY with a negative prefix argument, M-- M-x sly,
  ;; and you can select a program from that list.
  (setq sly-lisp-implementations
        `((sbcl (,(executable-find "sbcl")))))

  ;; Ensure history file exists
  (let ((history-file (expand-file-name "var/sly/mrepl-history"
                                        gx-xdg-cache-home)))
    (make-directory (file-name-directory history-file) t)
    (unless (file-exists-p history-file)
      (write-region "" nil history-file)))

  ;; Open Sly mREPL in background
  (setq display-buffer-alist
        (cons '("\\*sly-mrepl"
                (display-buffer-no-window)
                (allow-no-window . t))
              display-buffer-alist))

  (add-to-list 'beframe-global-buffers
               "\\*sly-mrepl")

  ;; See: https://joaotavora.github.io/sly/#Loading-Slynk-faster
  (defun gx/sly-auto-connect ()
    (interactive)
    (unless (sly-connected-p)
      (save-excursion (sly)))))

;; Neotree for Lem-like lisp IDE
(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 35
        neo-mode-line-type 'none
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t)

  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))


  ;; truncate long file names in neotree
  (add-hook 'neo-after-create-hook
            #'(lambda (_)
                (with-current-buffer (get-buffer neo-buffer-name)
                  (setq truncate-lines t)
                  (setq word-wrap nil)
                  (make-local-variable 'auto-hscroll-mode)
                  (setq auto-hscroll-mode nil)))))



;;; Guile Scheme IDE

;; Set default to guile.
(gx/setopts scheme-program-name "guile")

;; `emacs-guix' dependencies:
;; emacs-bui, emacs-dash, emacs-edit-indirect,
;; emacs-geiser, emacs-geiser-guile, emacs-magit-popup
;; module-import-compiled
(use-package guix
  :disabled)

(use-package arei
  :disabled
  :after (sesman)
  :config
  (require 'cl-lib)

  ;; Prevent `geiser' from interfering into completion (CAPF)
  (setq geiser-mode-auto-p nil)

  (defvar gx/ares-rs--process nil
    "Holds process for Ares nREPL RPC server.")

  (defun get-project-root-or-cwd ()
    "Get Project Root or Current working directory"
    (or (project-root (project-current))
        default-directory))

  (defun gx/kill-ares-nrepl ()
    "Kill Ares RS nREPL RPC server."
    (interactive)
    (when gx/ares-rs--process
      (ignore-errors
        (kill-process gx/ares-rs--process)
        (let ((port-file (expand-file-name
                          (concat (get-project-root-or-cwd)
                                  ".nrepl-port"))))
          (when (file-exists-p port-file)
            (delete-file port-file))))
      (setq gx/ares-rs--process nil)))

  (defun gx/ares-nrepl-start ()
    "Start Ares nREPL RPC server in Project Root or CWD."
    (interactive)

    (let* ((path (get-project-root-or-cwd))
           (bname (concat "*" (symbol-name (gensym "ares-nrepl-process-")) "*")))
      (gx/kill-ares-nrepl)
      (setq gx/ares-rs--process
            (start-process-shell-command
             bname
             (get-buffer-create bname)
             (concat "cd " path " && "
                     "ares-nrepl "
                     " -- "
                     "-L " path)))
      ;; Automatically start sesman session
      (when gx/ares-rs--process
        (ignore-errors
         (sesman-link-with-least-specific))))))





(provide 'gx-clide)
;;; gx-clide.el ends here
