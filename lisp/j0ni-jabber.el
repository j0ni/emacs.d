;;; j0ni-jabber.el -- jabber configuration

(package-require 'jabber)

(progn
  (require 'starttls)

  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments '("--insecure"))

  (setq jabber-account-list '(("13651_1189713@chat.hipchat.com")))

  (defvar hipchat-number "13651")
  (defvar hipchat-nickname "Jonathan Irving")

  (defun hipchat-join (room)
    (interactive "sRoom name: ")
    (jabber-groupchat-join
     (jabber-read-account)
     (concat hipchat-number "_" room "@conf.hipchat.com")
     hipchat-nickname
     t))

  ;; Mention nicknames in a way that HipChat clients will pickup
  (defun hipchat-mention (nickname)
    (interactive
     (list (jabber-muc-read-nickname jabber-group "Nickname: ")))
    (insert (concat "@\"" nickname "\" ")))


;; (setq jabber-account-list '(("jonathan.irving@turn.com"
  ;;                              (:network-server . "conf.turn.com")
  ;;                              ;; (:password . "tf@C^ng6")
  ;;                              (:connection-type . starttls))
  ;;                             ("jonathan.irving@gmail.com"
  ;;                              (:network-serer . "talk.google.com")
  ;;                              (:connection-type . ssl)
  ;;                              (:password . "ajlhiqdndvfejfuh"))
  ;;                             ("j@lollyshouse.ca"
  ;;                              (:connection-type . starttls)
  ;;                              (:password . "tf@C^ng6")))
  ;; jabber-muc-autojoin '("automatons@conference.turn.com")

  )

(provide 'j0ni-jabber)
