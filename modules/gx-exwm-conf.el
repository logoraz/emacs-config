;;; gx-exwm-conf.el --- EXWM Configuration File -*- lexical-binding: t -*-

;;; Commentary:
;;;


;;; Code:



;;; Configure Multiple Monitors
;; https://systemcrafters.net/emacs-desktop-environment/using-multiple-monitors/

(gx->defhook gx/update-displays
  "Handle display connectivity changes."
  (;;function body
   (gx/run-in-background "autorandr --change --force")
   (message "Display config: %s"
            (string-trim (shell-command-to-string "autorandr --current"))))
  :disable? t
  :hook exwm-randr-screen-change-hook)



;;; Notifications
;; #:TODO/2 - configure emacs-ednc
;; see: https://github.com/sinic/ednc
(use-package ednc
  :config
  (gx->defhook gx/ednc-minibuffer-present-hookfn
    "Present EDNC NEW notification in the minibuffer."

    (;;function body
     (when new
       (message "%s" (ednc-notification-summary new))))

    :hook ednc-notification-presentation-functions
    :args (old new))

  (ednc-mode 1))



;;; Desktop Environment

(use-package desktop-environment
  :after (exwm)
  :custom
  (desktop-environment-screenshot-partial-command
   "scrot -s -f"
   "Prevent glitching of selection box.")
  (desktop-environment-screenshot-directory "~/pictures/screenshots/")
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")
  :config (desktop-environment-mode))

(use-package exwm-modeline
  :ensure (exwm-modeline :pin melpa)
  :after (exwm)
  :custom
  (exwm-modeline-short t "Prefer short exhibit for workspace"))



;;; Configure EXWM

(use-package exwm
  :custom
  (exwm-workspace-number 6 "Set default number of workspaces.")
  ;; todo prep for multiple displays...
  ;; (exwm-randr-workspace-monitor-plist '(2 "Virtual-2" 3 "Virtual-2"))
  ;; (exwm-workspace-warp-cursor t)
  :config
  ;; Focus follows mouse
  (gx/setopts mouse-autoselect-window t
              focus-follows-mouse t)

  (defun gx/tab-bar--faces ()
    "Set custom faces for tab-bar"
    (set-face-attribute 'tab-bar nil
                        :foreground "#7b88a1")

    (set-face-attribute 'tab-bar-tab nil
                        :inherit 'tab-bar
                        :foreground "#b48ead")

    (set-face-attribute 'tab-bar-tab-inactive nil
                        :foreground "#7b88a1"))

  ;; Ensure custom faces take effect ALWAYS!
  (setopt tab-bar-new-tab-choice
          (lambda ()
            (gx/tab-bar--faces)
            (get-buffer "*scratch*")))

  (gx->defhook gx/tab-bar-face-hook
    "Hook to ensure tab-bar faces are set - sort of a hack."
    (;;function body
     (gx/tab-bar--faces))
    :hook window-configuration-change-hook
    :depth 95)

  (defun gx/exwm--modeline-status-bar ()
    "Configure Doom Modeline to act as status bar."
    (gx/setopts display-time-day-and-date t
                display-time-interval 1)
    (display-battery-mode 1)
    (display-time-mode 1)
    (exwm-modeline-mode))

  (defun gx/exwm--modeline-max ()
    "Reset modeline to minimal for truncated windows."
    (display-battery-mode 1)
    (display-time-mode 1))

  (defun gx/exwm--modeline-min ()
    "Reset modeline to minimal for truncated windows."
    (display-battery-mode -1)
    (display-time-mode -1))

  (gx->defhook gx/exwm--init-hookfn
    "Initialize workspace landing & tray items."

    (;;function body
     ;; Enable Window Dividers
     (window-divider-mode)

     ;; Start off with modeline as status bar
     (gx/exwm--modeline-status-bar)

     ;; Enable tab-bar mode & update faces
     (tab-bar-mode)
     (gx/tab-bar--faces)

     ;; Tray Apps
     (gx/run-in-background "blueman-applet")
     (gx/run-in-background "nm-applet"))

    :hook exwm-init-hook
    :depth 90)

  (gx->defhook gx/exwm--modeline-status-bar-init
    "Configure Doom Modeline to act as status bar when window equals frame."

    (;;function body
     (gx->defhook gx/exwm--modeline-status-bar-hookfn
       "Configure Doom Modeline to act as status bar when window equals frame."

       ;; Dynamic modeline status bar
       ;; Not sure how efficient this is, but works for now...
       ;; modeline returns to non status bar when window width < frame width
       (;;function body
        (if (eq (frame-width) (window-width))
            (gx/exwm--modeline-max)
          (gx/exwm--modeline-min)))

       :hook (window-size-change-functions exwm-workspace-switch-hook)
       :depth 95
       :args (&optional frame))

     (message "EXWM dynamic modeline status bar enabled."))

    :hook exwm-init-hook
    :depth 95
    :defer 3)

  (gx->defhook gx/exwm--update-class-hookfn
    "Update EXWM Class Name."

    (;;function body
     (exwm-workspace-rename-buffer exwm-class-name))

    :hook exwm-update-class-hook
    :depth 90)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        `(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j ;; Buffer list ?
          ?\C-\    ;; C-space
          ?\C-g
          ;; "Desktop" keys that need to pass through
          ,(kbd "<XF86MonBrightnessUp>")
          ,(kbd "<XF86MonBrightnessDown>")
          ,(kbd "<XF86AudioRaiseVolume>")
          ,(kbd "<XF86AudioLowerVolume>")
          ,(kbd "<XF86AudioMute>")
          ,(kbd "<XF86AudioPlay>")
          ,(kbd "<XF86AudioPause>")
          ,(kbd "<XF86AudioPrev>")
          ,(kbd "<XF86AudioNext>")
          ,(kbd "<print>")
          ,(kbd "S-<print>")))

  (let ((gx--exwm-mode-map
         `(("<XF86MonBrightnessUp>" . desktop-environment-brightness-increment)
           ("<XF86MonBrightnessDown>" . desktop-environment-brightness-decrement)
           ("<XF86AudioRaiseVolume>" . desktop-environment-volume-increment)
           ("<XF86AudioLowerVolume>" . desktop-environment-volume-decrement)
           ("<XF86AudioMute>" . desktop-environment-toggle-mute)
           ("<XF86AudioPlay>" . desktop-environment-music-toggle-command)
           ("<XF86AudioPause>" . desktop-environment-music-toggle-command)
           ("<XF86AudioPrev>" . desktop-environment-music-previous)
           ("<XF86AudioNext>" . desktop-environment-music-next)
           ("<print>" . desktop-environment-screenshot)
           ("S-<print>" . desktop-environment-screenshot-part))))
    (dolist (keybinding gx--exwm-mode-map)
      (define-key exwm-mode-map (kbd (car keybinding)) (cdr keybinding))))

  ;; #:TODO/250901 - Enhance simulation keybindings
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])))

  ;; Ctrl+q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(,@exwm-input-global-keys
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows/buffers
          ([s-left]  . windmove-left)
          ([s-right] . windmove-right)
          ([s-up]    . windmove-up)
          ([s-down]  . windmove-down)

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)

          ;; `M-N': Send to a certain workspace, Meta (Alt) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "M-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-move-window ,i))))
                    (number-sequence 0 9))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (defun gx/exwm--app-launcher (command)
    "Launch applications via shell command."
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))

  (exwm-input-set-key (kbd "s-SPC") 'gx/exwm--app-launcher)
  (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)
  (exwm-input-set-key (kbd "C-c t") 'tab-bar-switch-to-next-tab)

  ;; Ensure screen updates with xrandr will refresh EXWM frames
  (exwm-randr-mode)
  ;; (gx/update-displays)

  ;; Simple System Tray
  (exwm-systemtray-mode)
  ;; (setq exwm-systemtray-height 32)

  ;; Enable EXWM
  (gx/ignore-messages
    (exwm-wm-mode)))





(provide 'gx-exwm-conf)
;;; gx-exwm-conf.el ends here
