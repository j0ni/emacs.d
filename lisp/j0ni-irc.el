;;; j0ni-irc.el -- IRC configuration

(package-require 'circe)
(require 'circe)

(setq circe-network-options
      `(("Freenode"
         :nick "j0ni"
         :channels ("#clojure" "#lisp" "#clojure-emacs" "#clojureTO" "#emacs")
         :nickserv-password ,freenode-password)
        ("GIMPNet"
         :host "irc.gimp.ca"
         :port 6697
         :nick "j0ni"
         :channels ("#dnalounge")
         :nickserv-password ,gimpnet-password
         :use-tls t)
        ("Gitter"
         :host "irc.gitter.im"
         :port 6697
         :nick "j0ni"
         :pass ,gitter-token
         :use-tls t)
        ("Lollyshouse Slack"
         :host "lollyshouse.irc.slack.com"
         :port 6697
         :nick "j0ni"
         :pass ,lollyshouse-slack
         :use-tls t)
        ("Cloudfront"
         :host "irc.cloudfront.net"
         :port 7778
         :nick "j0ni"
         :channels ()
         :nickserv-password ,cloudfront-password)
        ("OFTC"
         :host "irc.oftc.net"
         :port 6697
         :nick "j0ni"
         :channels ("#torontocrypto" "#cryptoparty")
         :nickserv-password ,oftc-password
         :use-tls t)
        ("Mozilla"
         :host "irc.mozilla.org"
         :port 6697
         :use-tls t
         :nick "j0ni"
         :channels ("#rust" "#rust-beginners"))))

(require 'circe-color-nicks)
(enable-circe-color-nicks)

(setq tracking-ignored-buffers '(("#clojure-emacs" circe-highlight-nick-face)
                                 ("#clojure" circe-highlight-nick-face)
                                 ("#lisp" circe-highlight-nick-face)
                                 ("#emacs" circe-highlight-nick-face)
                                 ("#rust" circe-highlight-nick-face)
                                 ("#rust-beginners" circe-highlight-nick-face)))

(setq circe-reduce-lurker-spam t)

(setq lui-time-stamp-position 'right-margin
      lui-fill-type nil)

(add-hook 'lui-mode-hook 'my-lui-setup)
(defun my-lui-setup ()
  (setq fringes-outside-margins t
        right-margin-width 7
        word-wrap t
        wrap-prefix "    "))

(defun circe-network-connected-p (network)
  "Return non-nil if there's any Circe server-buffer whose
`circe-server-netwok' is NETWORK."
  (catch 'return
    (dolist (buffer (circe-server-buffers))
      (with-current-buffer buffer
        (if (string= network circe-server-network)
            (throw 'return t))))))

(defun circe-maybe-connect (network)
  "Connect to NETWORK, but ask user for confirmation if it's
already been connected to."
  (interactive "sNetwork: ")
  (if (or (not (circe-network-connected-p network))
          (y-or-n-p (format "Already connected to %s, reconnect?" network)))
      (circe network)))

(defun start-irc ()
  "Connect to IRC"
  (interactive)
  (circe-maybe-connect "Freenode")
  (circe-maybe-connect "GIMPNet")
  (circe-maybe-connect "Mozilla"))

(defun my-circe-message-option-chanserv (nick user host command args)
  (when (and (string= "ChanServ" nick)
             (string-match "^\\[#.+?\\]" (cadr args)))
    '((dont-display . t))))

(add-hook 'circe-message-option-functions 'my-circe-message-option-chanserv)

(defun my-circe-chanserv-message-handler (nick user host command args)
  (when (and (string= "ChanServ" nick)
             (string-match "^\\[\\(#.+?\\)\\]" (cadr args)))
    (let* ((channel (match-string 1 (cadr args)))
           (buffer (circe-server-get-chat-buffer channel t)))
      (let ((circe-server-last-active-buffer buffer))
        (circe-display-NOTICE nick user host command args)))))

;; (circe-add-message-handler "NOTICE" 'my-circe-chanserv-message-handler)

(provide 'j0ni-irc)
