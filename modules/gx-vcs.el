;;; gx-vcs.el --- CL IDE -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:


(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git))  ; Only use Git backend
  :bind
  (("C-x g" . vc-dir)
   ("C-c g s" . vc-dir)
   ("C-c g l" . vc-print-log)
   ("C-c g b" . vc-annotate)
   ;; Add push binding
   ("C-x v P" . vc-push))
  :config
  ;; Open vc-dir in same window
  (add-to-list 'display-buffer-alist
               '("\\*vc-dir\\*"
                 (display-buffer-same-window))))

;; Better diff colors
(use-package diff-mode
  :ensure nil
  :custom
  (diff-font-lock-prettify t))

;; Git-specific settings
(use-package vc-git
  :ensure nil
  :after vc
  :bind
  (:map vc-dir-git-mode-map
        ("r i" . gx/vc-git-rebase))
  :custom
  (vc-git-diff-switches '("-w"))  ; Ignore whitespace in diffs
  (vc-git-print-log-follow t)     ; Follow file renames in log
  :config
  (defun gx/vc-git-rebase (commit)
    "Interactive rebase from COMMIT"
    (interactive "sRebase from (HEAD~N or commit): ")
    (let ((default-directory (vc-root-dir)))
      (async-shell-command (format "git rebase -i %s" commit)))))

;; Git Rebase Mode
(use-package git-rebase
  :if (eq system-type 'gnu/linux)
  :ensure nil)

(use-package git-modes
  :if (eq system-type 'windows-nt)
  :ensure t
  :config
  (setq git-rebase-show-instructions nil))


;; Fall back to external Version Control via Magit
(unless (eq system-type 'windows-nt)
  (use-package magit
    :disabled
    :defer 2
    :ensure (magit :pin melpa)
    :custom
    (magit-clone-always-transient nil)
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (vc-follow-symlinks t)
    :config
    (setq auto-revert-verbose nil)
    (gx/ignore-messages
      (global-auto-revert-mode))))





(provide 'gx-vcs)
;;; gx-vcs.el ends here
