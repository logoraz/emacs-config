;;; gx-desktop.el --- Emacs Desktop Commands -*- lexical-binding: t -*-

;;; Commentary:
;;;


;;; Code:

(require 'rx)



;;; Keyboard backlight

(defun gx/keyboad-brightness-increase ()
  "Increase keyboard backlight by 10%."
  (interactive)
  (gx/run-command-with-output
   "brightnessctl -d chromeos::kbd_backlight s +10%"
   (rx (: (1+ (any "0-9")) "%"))
   "Keyboard backlight set to"))

(defun gx/keyboad-brightness-decrease ()
  "Decrease keyboard backlight  by 10%."
  (interactive)
  (gx/run-command-with-output
   "brightnessctl -d chromeos::kbd_backlight s -10%"
   (rx (: (1+ (any "0-9")) "%"))
   "Keyboard backlight set to"))

(defun gx/keyboad-brightness-off ()
  "Turn off keyboard backlight."
  (interactive)
  (gx/run-command-with-output
   "brightnessctl -d chromeos::kbd_backlight s 0%"
   (rx (: (1+ (any "0-9")) "%"))
   "Keyboard backlight set to"))





(provide 'gx-desktop)
;;; gx-desktop.el ends here
