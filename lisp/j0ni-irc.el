;;; j0ni-irc.el -- IRC configuration

(package-require 'circe)
(require 'circe)

(setq circe-network-options
      `(("Tunnelled Freenode"
         :host "localhost"
         :port 4222
         :pass ,proxy-password)
        ("Tunnelled GIMPNet"
         :host "localhost"
         :port 4223
         :pass ,proxy-password)
        ("Freenode"
         :nick "j0nii"
         :channels ("#clojure" "#elixir-lang" "#lisp" "#clojure-emacs" "#clojureTO" "#emacs")
         :nickserv-password ,freenode-password)
        ("GIMPNet"
         :host "irc.gimp.ca"
         :port 6697
         :nick "j0nii"
         :channels ("#dnalounge")
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
         :nick "j0nii"
         :channels ("#torontocrypto" "#cryptoparty")
         :nickserv-password ,oftc-password
         :use-tls t)))

(setq tracking-ignored-buffers '(("#clojure-emacs" circe-highlight-nick-face)
                                 ("#clojure" circe-highlight-nick-face)
                                 ("#elixir-lang")
                                 ("#lisp" circe-highlight-nick-face)
                                 ("#emacs" circe-highlight-nick-face)
                                 ("#compojure" circe-highlight-nick-face)
                                 ("#Node.js" circe-highlight-nick-face)))

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

(defun irc ()
  "Connect to IRC"
  (interactive)
  (circe-maybe-connect "Freenode")
  (circe-maybe-connect "GIMPNet")
  ;; (circe-maybe-connect "Tunnelled Freenode")
  ;; (circe-maybe-connect "Tunnelled GIMPNet")
  )

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
