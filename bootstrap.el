;;; bootstrap.el --- Bootstrap Emacs Config -*- lexical-binding: t -*-

;;; Commentary:
;;;
;;; Run `emacs --script bootstrap.el'


;;; Code:

(defun gx/create-symlink (target link)
  "Create a symlink, LINK of TARGET."
  (let ((target (expand-file-name target))
        (link (expand-file-name link)))
    (when (file-exists-p link)
      (delete-file link))
    (make-symbolic-link target link)))


;;; Backup System for configs
(defun gx/archive-old-configs ()
  "Rename old Emacs config files and directories to avoid conflicts."
  (let ((old-files '("~/.emacs" "~/.emacs.el"))
        (old-dir "~/.emacs.d")
        (timestamp (format-time-string "%Y%m%d-%H%M%S")))

    ;; Handle old config files
    (dolist (file old-files)
      (when (file-exists-p file)
        (let ((backup (format "%s.old-%s" file timestamp)))
          (rename-file file backup)
          (message "Renamed %s to %s" file backup))))

    ;; Handle .emacs.d directory
    (when (file-exists-p old-dir)
      (let ((backup (format "%s.old-%s" old-dir timestamp)))
        (rename-file old-dir backup)
        (message "Renamed %s to %s" old-dir backup)))))


;;; Deploy
(defun gx/deploy-config ()
  "Deploy gxEmacs config."
  (message "Starting gxEmacs configuration bootstrap...")
  (gx/archive-old-configs)
  (gx/create-symlink "~/Work/emacs-config" "~/.config/emacs")
  (message "Bootstrap complete!"))

(defun gx/deploy-sbclrc ()
  "Copy reference sbclrc, SOURCE, to DESTINATION."
  (pcase system-type
    ('windows-nt (gx/create-symlink
                  (expand-file-name "dot-sbclrc-windows.lisp"
                                    "~/.emacs.d/files/common-lisp")
                  (expand-file-name ".sbclrc" "~")))
    ('gnu/linux (gx/create-symlink
                 (expand-file-name "dot-sbclrc-linux.lisp"
                                   "~/Work/emacs-config/files/common-lisp")
                 (expand-file-name ".sbclrc" "~"))))
  (message "SBCLRC deployed"))

(defun gx/deploy-eclrc ()
  "Copy reference eclrc, SOURCE, to DESTINATION."
  (pcase system-type
    ('gnu/linux (gx/create-symlink
                 (expand-file-name "dot-eclrc-linux.lisp"
                                   "~/Work/emacs-config/files/common-lisp")
                 (expand-file-name ".eclrc" "~"))))
  (message "ECLRC deployed"))


(if (eq system-type 'gnu/linux) (gx/deploy-config))

(when (eq system-type 'gnu/linux)
  (gx/deploy-sbclrc)
  (gx/deploy-eclrc))





(provide 'bootstrap)
;;; bootstrap.el ends here
