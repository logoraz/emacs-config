;;; gx-exwm-init.el --- EXWM X Initialization File -*- lexical-binding: t -*-

;;; Commentary:
;;;


;;; Code:

(require 'rx)



;;; Custom Hardware Controls

;;; Keyboard backlight
;; brightnessctl --list
;; Device 'chromeos::kbd_backlight' of class 'leds':
;;         Current brightness: 20 (20%)
;;         Max brightness: 100

(defun gx/keyboad-brightness-increase ()
  "Increase keyboard backlight by 10%."
  (interactive)
  (start-process-shell-command
   "keyboard" nil
   "brightnessctl -d chromeos::kbd_backlight s +10%"))

(defun gx/keyboad-brightness-decrease ()
  "Decrease keyboard backlight  by 10%."
  (interactive)
  (start-process-shell-command
   "keyboard" nil
   "brightnessctl -d chromeos::kbd_backlight s -10%"))

(defun gx/keyboad-brightness-off ()
  "Turn off keyboard backlight."
  (interactive)
  (start-process-shell-command
   "keyboard" nil
   "brightnessctl -d chromeos::kbd_backlight s 0%"))


;;; Control Mouse tracking speed

(defun gx/get--mouse-id ()
  "Dynamically retrieve mouse id from xinput.
Returns Touchpad ID as string.

Currently tailored to retreive ID of Logitech Hero Gaming Mouse.

*Only* works if this specific mouse is plugged in when starting up."

  ;; #:TODO/250829 --> Clean-up/Refactor (pretty ugly as of now, but works!)
  ;; #:TODO/250921 --> Generalize to get ID of any mouse plugged in?
  (let ((mouse-rx (rx (: "Gaming Mouse " (not (any "Keyboard")) (* nonl))))
        (filter-rx (rx "id=" (+ (any "0-9"))))
        (id-rx (rx (+ (any "0-9"))))
        (mouse-id "")
        (match-str ""))
    (dolist (line (process-lines "xinput" "list") mouse-id)
      (when (string-match mouse-rx line)
        (setq match-str (match-string 0 line))
        (when (string-match filter-rx match-str)
          (setq match-str (match-string 0 match-str))
          (when (string-match id-rx match-str)
            (setq mouse-id (match-string 0 match-str))))))))

(defun gx/accelerate--mouse ()
  "Increase mouse tracking speed."
  (interactive)
  ;; #:TODO/250921 need to generalize/check if mouse-id is valid.
  (start-process-shell-command
   "mouse" nil (concat "xinput "
                       "--set-prop "
                       (gx/get--mouse-id)
                       " 'libinput Accel Speed' 1")))


;;; Disable Trackpad

(defvar gx/touchpad--state t
  "Hold boolean value of touchpad state, is always on by default.")

(defun gx/get--touchpad-id ()
  "Dynamically retrieve touchpad id from xinput.
Returns Touchpad ID as string."

  ;; #:TODO/250829 --> Clean-up/Refactor (pretty ugly as of now, but works!)
  (let ((touchpad-rx (rx (: "Touchpad" (* nonl))))
        (filter-rx (rx "id=" (= 2 (any "0-9"))))
        (id-rx (rx (= 2 (any "0-9"))))
        (touchpad-id "")
        (match-str ""))
    (dolist (line (process-lines "xinput" "list") touchpad-id)
      (when (string-match touchpad-rx line)
        (setq match-str (match-string 0 line))
        (when (string-match filter-rx match-str)
          (setq match-str (match-string 0 match-str))
          (when (string-match id-rx match-str)
            (setq touchpad-id (match-string 0 match-str))))))))

(defun gx/set--touchpad-state (&optional state)
  "Enable/Disable touchpad provided STATE."
  (unless state (setq state "disable"))
  (start-process-shell-command
   "touchpad" nil (concat "xinput "
                          "-" state " "
                          (gx/get--touchpad-id))))

(defun gx/toggle-touchpad ()
  "Toggle touchpad control..."
  (interactive)
  (gx/set--touchpad-state (if gx/touchpad--state "disable" "enable"))
  (setq gx/touchpad--state (not gx/touchpad--state)))



;;; X11 Environment Setup

(defun gx/set-wallpaper ()
  "Set EXWM wallpaper"
  (start-process-shell-command
   "wallpaper" nil "feh --bg-scale ~/pictures/wallpapers/desktop-bg.jpg"))

(defun gx/set-x-environment ()
  "Set the X environment."

    ;; Turn off system bell & screen-saver control
  (gx/run-in-background "xset b off")
  (gx/run-in-background "xset s off")

  ;; Set X11 Root Cursor
  (gx/run-in-background "xsetroot -cursor_name left_ptr")

  ;; UI Settings
  ;; Load X11  Resources
  (start-process-shell-command
   "xresources" nil "xrdb -merge ~/.Xresources")

  ;; Set the screen resolution
  ;; (start-process-shell-command "xrandr" nil "")
  (gx/run-in-background "xrandr --output eDP --set TearFree on")

  (gx/set-wallpaper)

  ;; Enable screen locking on suspend
  (gx/run-in-background "xss-lock -- slock")

  ;; Enable Screen compositing
  (gx/run-in-background "picom --backend xrender")

  (gx/run-in-background "brightnessctl set 50%")

  ;; Disable Touchpad at start
  (gx/toggle-touchpad)

  ;; Accelerate Mouse tracking speed at start
  ;; only applies if mouse is plugged in when starting
  (gx/accelerate--mouse))

;; Set X environment upfront before loading EXWM
(gx/set-x-environment)





(provide 'gx-exwm-init)
;;; gx-exwm-init.el ends here
