;;; gx-vcs.el --- CL IDE -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;;; Git Porcelain
(use-package magit
  :defer t
  :ensure (magit :pin melpa)
  :custom
  (magit-clone-always-transient nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (vc-follow-symlinks t)
  :config
  (setq auto-revert-verbose nil)
  (gx/ignore-messages
    (global-auto-revert-mode)))


;;; Emacs VCS fallback configuration
(use-package vc
  :disabled
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)) ; Only use Git backend
  :bind
  (("C-x g"   . vc-dir)
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
  :disabled
  :ensure nil
  :custom
  (diff-font-lock-prettify t))

;; Git-specific settings
(use-package vc-git
  :disabled
  :ensure nil
  :after vc
  :bind
  (:map vc-dir-git-mode-map
        ("r i" . gx/vc-git-rebase)
        ("P"   . vc-push)
        ("U"   . vc-pull)
        ("F"   . gx/vc-git-push-force))
  :custom
  (vc-git-diff-switches '("-w"))  ; Ignore whitespace in diffs
  (vc-git-print-log-follow t)     ; Follow file renames in log
  :config
  (defun gx/vc-git-rebase (commit)
    "Interactive rebase from COMMIT"
    (interactive "sRebase from (HEAD~N or commit): ")
    (let ((default-directory (vc-root-dir)))
      (async-shell-command (format "git rebase -i %s" commit))))

  (defun gx/vc-git-push-force ()
    "Force push to remote."
    (interactive)
    (let ((default-directory (vc-root-dir)))
      (shell-command "git push --force-with-lease"))))





(provide 'gx-vcs)
;;; gx-vcs.el ends here
