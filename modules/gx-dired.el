;;; gx-dired.el --- Advanced Configuration for Dired -*- lexical-binding: t -*-

;;; Commentary:
;;;


;;; Code:



;;; Main Dired setup

(use-package dired
  :defer 2)

(use-package dired-x
  :defer 2
  ;; package provides dired-jump (C-x C-j)
  :after (dired)
  ;; :hook (dired-mode . dired-omit-mode)
  :custom (dired-x-hands-off-my-keys nil)
  :config
  ;; (setq dired-omit-files   ;; hide .dot files when in dired-omit-mode
  ;;     (concat dired-omit-files "\\|^\\..+$"))
  )

(use-package image-dired
  :defer 2
  :custom ((image-dired-thumb-size 256)
           (image-dired-thumbnail-storage 'standard-large)))




;;; DIRED Extensions --> Prettify & Mutimedia Support

(use-package all-the-icons-dired
  :defer 2
  :ensure t
  ;; :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package dired-preview
  :defer 2
  :ensure t
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
  :defer 5
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
