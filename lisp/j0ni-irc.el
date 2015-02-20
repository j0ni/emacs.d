;;; j0ni-irc.el -- IRC configuration

(package-require 'circe)
(require 'circe)

(setq circe-network-options
      `(("Freenode"
         :nick "j0nii"
         :channels ("#emacs" "#clojure" "#lisp" "#clojure-emacs")
         :nickserv-password ,freenode-password)
        ("GIMPNet"
         :host "irc.gimp.net"
         :port (6667 . 6697)
         :nick "j0nii"
         :channels ("#dnalounge"))))

(setq tracking-ignored-buffers '(("#emacs" circe-highlight-nick-face)
                                 ("#clojure-emacs" circe-highlight-nick-face)
                                 ("#clojure" circe-highlight-nick-face)
                                 ("#lisp" circe-highlight-nick-face)))

(setq circe-reduce-lurker-spam t)

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
  (circe-maybe-connect "GIMPNet"))

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

(circe-add-message-handler "NOTICE" 'my-circe-chanserv-message-handler)

(provide 'j0ni-irc)
