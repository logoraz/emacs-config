;;; bootstrap.el --- Bootstrap Emacs Config -*- lexical-binding: t -*-

;;; Commentary:
;;;
;;; Run `emacs --script bootstrap.el'


;;; Code:



;;; Functions for symlinking and backup of existing "old" configs
(defun gx/create-config-dir-link (target link)
  "Create a symlink, LINK of TARGET, to my config file."
  (let ((target (expand-file-name target))
        (link (expand-file-name link)))
    (when (file-exists-p link)
      (delete-file link))
    (make-symbolic-link target link)))

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


;;; Execute/Deploy
(message "Starting gxEmacs configuration bootstrap...")
(gx/archive-old-configs)
(gx/create-config-dir-link "~/Work/emacs-config" "~/.config/emacs")
(message "Bootstrap complete!")




(provide 'bootstrap)
;;; bootstrap.el ends here
