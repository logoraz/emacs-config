;;; early-init.el --- Early Initialization File -*- lexical-binding: t -*-

;;; Commentary:
;;;
;;;


;;; Code:



;;; Bootstrap
(defvar gx-xdg-config-home (expand-file-name "emacs" "~/.config")
  "Emacs XDG_CONFIG_HOME path.")

(defvar gx-xdg-cache-home (expand-file-name "emacs" "~/.cache")
  "Emacs XDG_CACHE_HOME path.")

(defvar gx-syntax-directory (expand-file-name "syntax" gx-xdg-config-home)
  "Emacs Syntax Extensions directory.")

;; Add language syntax expression to load path and use
(add-to-list 'load-path gx-syntax-directory)
(require 'gx-subrx) ;; Always byte-compile this module!

(gx/use-modules package)

;; Set the `user-emacs-directory` to a writeable path
(setq user-emacs-directory gx-xdg-cache-home)



;;; Compilation Settings

(gx/setopts load-prefer-newer t
            "Always load newer native comp files"
            warning-suppress-log-types '((comp) (initialization))
            "Do not log warnings for compilation & initialization."
            warning-suppress-types '((initialization))
            "Do not display initialization warning types.")

(when (featurep 'native-compile)
  ;; Set native compilation asynchronous
  (setq-default native-comp-jit-compilation t)
  (gx/setopts native-comp-async-report-warnings-errors nil
              "Suppress native comp warnings")
  ;; Set the right directory to store the native compilation cache
  ;; NOTE: The method for setting the eln-cache directory depends on the emacs
  ;; version. This is disregarded here - assume I always use emacs latest,
  ;; i.e. version >= 29
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))



;;; Performance Optimizations (Hacks)

;; Disable Dialogs/Echos/Bells & Startup Frames/Screens/Buffers
(setq-default init-file-user user-login-name)

(gx/setopts
 frame-inhibit-implied-resize 'force
 "Disable with force, critical to smooth startup."
 ring-bell-function 'ignore
 "No need for noisy notifications at startup, doom-modeline set it later."
 use-file-dialog nil
 "Disable file dialog."
 use-dialog-box nil
 "Disable dialog box."
 inhibit-startup-screen t
 "Disable splash screen."
 inhibit-startup-echo-area-message user-login-name
 "Disable startup echo area message."
 inhibit-startup-buffer-menu t
 "Disable startup buffer menu.")

;; Inhibit redisplay & messaging/dialog/echo to avoid flickering
;; loading/compiling upon iniial startup etc.
;; re-instantiate after init.el --> gx--lazarus-hookfn
(setq inhibit-redisplay t
      inhibit-message t)


;; Temporarily increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes (* 8 100 1000).
;; Reset GC to default after start-up --> gx--lazarus-hookfn
(defvar gx-gc-cons-threshold gc-cons-threshold
  "Capture the default value of `gc-cons-threshold' for restoration.")
(setq gc-cons-threshold most-positive-fixnum)

;; Temporarily disable file-handling during startup.
(defvar gx-file-name-handler-alist file-name-handler-alist
  "Capture the default value of `file-name-handler-alist' for restoration.")
(setq file-name-handler-alist nil)

;; Reduce `vc-handled-backends' to only Git for I/O optimization.
(defvar gx-vc-handled-backends vc-handled-backends
  "Capture the default value of `vc-handled-backends' for restoration.")
(gx/setopts vc-handled-backends '(Git) "Set Git to be the only VC for now.")

;; Restore Emacs Defaults after initialization
(gx->defhook gx/lazarus--hookfn
  "Ressurect Emacs 'Defaults' hacked to optimize startup in `early-init'."

  (;;function body
   (setq file-name-handler-alist gx-file-name-handler-alist)
   (gx/setopts gc-cons-threshold gx-gc-cons-threshold
               "Restore GC Threshold to default.")

   ;; Restore messages & redisplay
   (setq inhibit-redisplay nil
         inhibit-message nil)

   (redisplay))

  :hook emacs-startup-hook
  :depth 90)



;;; Set Default UI/UX Configuration Variables

;; See Window Frame parameters
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/
;; Window-Frame-Parameters.html
(set-frame-name "Home")

;; Customize Frame Title Construct
(setq-default frame-title-format
              '(multiple-frames
                "%b"
                ("" "%b @" user-login-name)))

(gx/setopts frame-resize-pixelwise t
            "Hopefully make resizing frame more smooth.")

(defvar gx--custom-frame-alist
  '((alpha-background . 0.85) ;; Need an X Emacs that is capable of this.
    (fullscreen . maximized)
    (use-frame-synchronization . extended))
  "Default frame parameters.")

(gx/setopts initial-frame-alist
            (append
             gx--custom-frame-alist
             initial-frame-alist)
            "Customize the initial frame alist.")

(gx/setopts default-frame-alist
            (append
             gx--custom-frame-alist
             default-frame-alist)
            "Customize the default frame alist.")

;; Avoid the flash of light
;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/
;; early-init.el
(defun gx/avoid-initial-flash-of-light ()
  "Improve Emacs startup appearance, normalize with theme - no white."
  (setq mode-line-format nil)
  (set-frame-parameter nil 'alpha-background 85)
  (set-face-attribute 'default nil
                      :background "#1d1f21" :foreground "#d8dee9")
  (set-face-attribute 'mode-line nil
                      :background "#1d1f21" :foreground "#d8dee9"
                      :box 'unspecified)
  (set-face-attribute 'mode-line-inactive nil
                      :background "#1d1f21" :foreground "#d8dee9"
                      :box 'unspecified))

;; Set Initial UI/UX Configuration for a clean startup experience
(gx/avoid-initial-flash-of-light)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)
(pixel-scroll-precision-mode 1)



;;; Package Management System & Loading Preferences
;; TODO - Determine how to handle package loading between
;;         `GUIX' and `package.el' and `use-package' abilities to leverage both
;;         systems.
;; https://www.reddit.com/r/emacs/comments/jhb2i6/
;; guix_the_right_way_to_manage_your_packages/

(gx/setopts package-enable-at-startup t
            "Enable for things to work, greatly impacts startup time."
            package-user-dir (expand-file-name "elpa" gx-xdg-cache-home)
            "Relocate elpa to Emacs XDG_CACHE_HOME location.")

(add-to-list 'package-archives
             '("stable" . "https://stable.melpa.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(gx/setopts package-archive-priorities
            '(("melpa"  . 99)  ;; prefer bleading-edge package from melpa
              ("stable" . 80)  ;; use stable "released" versions next
              ("nongnu" . 70)  ;; use non-gnu package if not found in melpa's
              ("gnu"    . 0))  ;; if all else fails, get it from gnu
            "Set package archive preference: melpa > stable > nongnu > gnu")

;; disable for now as it greatly impacts startup!
(package-initialize)





(provide 'early-init)
;;; early-init.el ends here
