;;; gx-dired.el --- Advanced Configuration for Dired -*- lexical-binding: t -*-

;;; Commentary:
;;;


;;; Code:



;;; Main Dired setup

(use-package dired)

(use-package dired-x
  ;; package provides dired-jump (C-x C-j)
  :after (dired)
  ;; :hook (dired-mode . dired-omit-mode)
  :custom (dired-x-hands-off-my-keys nil)
  :config
  ;; (setq dired-omit-files   ;; hide .dot files when in dired-omit-mode
  ;;     (concat dired-omit-files "\\|^\\..+$"))
  )

(use-package image-dired
  :custom ((image-dired-thumb-size 256)
           (image-dired-thumbnail-storage 'standard-large)))




;;; DIRED Extensions --> Prettify & Mutimedia Support

(use-package all-the-icons-dired
  ;; :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package dired-preview
  :after (dired image-dired)
  ;; https://protesilaos.com/emacs/dired-preview
  :hook ((dired-preview-mode . dired-hide-details-mode)
         (dired-preview-mode . all-the-icons-dired-mode)
         (dired-preview-mode . ready-player-mode))
  :bind (:map dired-mode-map
              ("C-c C-p" . dired-preview-mode)
              ("C-c C-k" . ready-player-mode))
  :config
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(gz\\|"
                "zst\\|"
                "tar\\|"
                "xz\\|"
                "rar\\|"
                "zip\\|"
                "iso\\|"
                "epub"
                "\\)")))

(use-package ready-player
  ;; currently not available in guix
  ;; https://github.com/xenodium/ready-player
  ;; For some reason use-package is not able to successfuly retreive/load
  ;; this unless I manually install from list-packages
  ;; --> melpa
  :ensure (ready-player :pin melpa)
  :custom ((ready-player-autoplay nil)
           (ready-player-thumbnail-max-pixel-height 500)))





(provide 'gx-dired)
;;; gx-dired.el ends here
