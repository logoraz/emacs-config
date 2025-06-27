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
;;   3. Look into:  `M-.' -> runs the command sly-edit-definition
;;      (found in sly-mode-map),
;;      which is an interactive native-compiled Lisp function in ‘sly.el’.


;; Code:

(use-package sly
  :ensure t
  ;; Enable sly IDE for Common Lisp
  :hook ((lisp-mode . sly-editing-mode))

  :custom
  (inferior-lisp-program (executable-find "ccl")
                         "Set default lisp to Steel Bank Common Lisp.")
  :config
  ;;Start the REPL by issuing M-- M-x sly RET nyxt RET and evaluate:
  ;; (asdf:load-system :nyxt/gi-gtk)
  ;; (nyxt:start)
  ;; TODO: determine how to start w/o loading config for this...
  (defvar *nyxt-env* (concat "CL_SOURCE_REGISTRY="
                             "~/.local/share/common-lisp/bin/nyxt//:"
                             "~/.local/share/common-lisp/bin/nyxt/ocicl//")
    "Setup environment to run Nyxt's CL system from SLY")

  ;; Invoke SLY with a negative prefix argument, M-- M-x sly,
  ;; and you can select a program from that list.
  (setq sly-lisp-implementations
        `((ccl   (,(executable-find "ccl"))   :coding-system utf-8-unix)
          (sbcl  (,(executable-find "sbcl"))  :coding-system utf-8-unix)
          (ecl   (,(executable-find "ecl"))   :coding-system utf-8-unix)
          (clisp (,(executable-find "clisp")) :coding-system utf-8-unix)
          (clasp (,(executable-find "clasp")) :coding-system utf-8-unix)
          ;; TODO - Updated: currently doesn't work for electron
          (nyxt ("sbcl" "--dynamic-space-size 3072")
                :env (,*nyxt-env*)
                :coding-system utf-8-unix)))

  (defun raz/nyxt-sly-connect ()
    "Connect to Nyxt slynk session, start via M-x start-slynk -> port 4005."
    (interactive)
    ;;FIXME -> query if nyxt-slynk is running or has been started (unless ...)
    (save-excursion (sly-connect "localhost" 4005)))

  ;; See: https://joaotavora.github.io/sly/#Loading-Slynk-faster
  ;; to be passed to `:hook' as `(lisp-mode . raz/sly-auto-connect)'
  (defun raz/sly-auto-connect ()
    ;; (interactive)
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
