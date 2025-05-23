;;; raz-lisp-ide --- Common Lisp IDE for RazEmacs, -*- lexical-binding: t; -*-

;; Author: Erik P. Almaraz

;;; Commentary:
;;

;; References
;; 1. http://joaotavora.github.io/sly/#A-SLY-tour-for-SLIME-users
;; 2. Source: https://github.com/joaotavora/sly
;;
;; TODO:
;;   1. Setup/Configure Nyxt extension
;;   2. Setup/Configure stumpwm-mode extension
;;   3. Look into:  `M-.' -> runs the command sly-edit-definition (found in sly-mode-map),
;;      which is an interactive native-compiled Lisp function in ‘sly.el’.


;; Code:

(use-package sly
  :ensure t
  ;; Enable sly IDE for Common Lisp
  :hook ((lisp-mode . sly-editing-mode))
  :custom
  (inferior-lisp-program (executable-find "sbcl")
                         "Set default lisp to Steel Bank Common Lisp.")
  :config
  ;; Invoke SLY with a negative prefix argument, M-- M-x sly,
  ;; and you can select a program from that list.
  (setq sly-lisp-implementations
        `((sbcl (,(executable-find "sbcl")))
          (clasp (,(executable-find "clasp")))
          (ecl (,(executable-find "ecl")))
          (ccl (,(executable-find "ccl")))))

  (defvar raz/sly-saved-win-config nil)

  (defun raz/sly-save-window-configuration ()
    (setq raz/sly-saved-win-config (current-window-configuration)))

  (defun raz/restore-window-configuration ()
    (set-window-configuration raz/sly-saved-win-config))

  ;; See: https://joaotavora.github.io/sly/#Loading-Slynk-faster
  ;; to be passed to `:hook' as `(lisp-mode . raz/sly-auto-connect)'
  (defun raz/sly-auto-connect ()
    (interactive)
    ;; (setq sly-contribs '(sly-scratch sly-mrepl sly-fancy))
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

;; Terminals to access tools like ocicl
(use-package vterm
  :ensure t)


(provide 'raz-lisp-ide)
