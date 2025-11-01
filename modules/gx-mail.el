;;; gx-mail.el --- Advanced eMail/chat Management -*- lexical-binding: t; -*-

;;; Commentary:
;;; FIXME: Configure GPG w/o having to use pinentry...


;;; Code:




(use-package mu4e
  :defer t          ;remove once hook is used for gpg encryption
  ;; :hook (message-send . 'mml-secure-message-sign-pgpmime)
  :custom
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (mu4e-update-interval (* 10 60))
  (mu4e-get-mail-command "mbsync -a")

  ;; Make sure plain text mails flow correctly for recipients
  (mu4e-compose-format-flowed t)

  ;; Configure the program & function to use for sending mail
  (sendmail-program (executable-find "msmtp"))
  ;; (message-send-mail-function 'sendmail-send-it)
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (message-kill-buffer-on-exit t)
  ;; TODO: Automatically sign all outgoing mails
  ;; Use a specific key for signing by referencing its thumbprint
  ;; (mml-secure-openpgp-signers '("BCAD6D0FE9E5C0E7"))
  :config
  ;; Set Signatures
  (defvar fastmail-signature (concat "\n\n"
                                     "Erik Almaraz \n"
                                     "sent via Mu4e"))

  ;; Set mu4e to use same window, as opposed to destroying your current window layout
  (setq display-buffer-alist '(("\\*mu4e-main\\*" display-buffer-same-window)))

  ;; Mail Accounts
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Fastmail"
          :enter-func
          (lambda () (mu4e-message "Enter erikalmaraz@fastmail.com context"))
          :leave-func
          (lambda () (mu4e-message "Leave erikalmaraz@fastmail.com context"))
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Fastmail" (mu4e-message-field msg :maildir))))
          :vars `((user-full-name         . "Erik Almaraz")
                  (user-mail-address      . "erikalmaraz@fastmail.com")
                  (mu4e-compose-signature . ,fastmail-signature)
                  (mu4e-drafts-folder     . "/Fastmail/Drafts")
                  (mu4e-sent-folder       . "/Fastmail/Sent")
                  (mu4e-refile-folder     . "/Fastmail/Archive")
                  (mu4e-trash-folder      . "/Fastmail/Trash")))))

  ;; Favorites
  ;; TODO
  (setq mu4e-maildir-shortcuts
        '(("/Fastmail/Inbox"     . ?i)
          ("/Fastmail/Archive"   . ?a)
          ("/Fastmail/Drafts"    . ?d)
          ("/Fastmail/Sent"      . ?s)
          ("/Fastmail/Scheduled" . ?e)
          ("/Fastmail/Spam"      . ?p)
          ("/Fastmail/Trash"     . ?t)
          ;; Custom maildir's
          ("/Fastmail/Craft"  . ?c)
          ("/Fastmail/Home"   . ?h)
          ("/Fastmail/News"   . ?n)
          ("/Fastmail/Note"   . ?r)
          ("/Fastmail/Trade"  . ?w))))



;;; Chat/ERC configuration
(setq erc-server "irc.libera.chat"
      erc-nick "logoraz"
      erc-user-full-name "Erik Almaraz"
      erc-track-shorten-start 8
      ;;    erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury
      erc-fill-function 'erc-fill-static
      ;;      erc-track-exclude-server-buffer t
      erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "AWAY" "PART")
      erc-hide-list '("JOIN" "NICK" "QUIT" "MODE" "AWAY" "PART")
      erc-pals '("daviwil" "Fade" "SummerEmacs" "benoitj" "BigEatie")
      erc-fill-column 75)


(use-package erc-hl-nicks
  :after (erc)
  :config
  (add-to-list 'erc-modules 'hl-nicks))

(use-package erc-image
  :after (erc)
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image))



;;; Mail/Chat Emojification

(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)





(provide 'gx-mail)
;;; gx-mail.el ends here
