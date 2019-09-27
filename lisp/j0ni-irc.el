;;; j0ni-irc.el -- IRC configuration

;; erc
(require 'erc)
;; (require 'erc-truncate)
;; (require 'notifications)
;; (require 'erc-pcomplete)
;; (require 'erc-button)
;; (require 'erc-join)

(setq erc-fill-column 100)
(setq erc-rename-buffers t)
(setq erc-nick "mhcat")
(setq erc-user-full-name "MH Cat")
(setq erc-password freenode-password)
(setq erc-track-exclude-types '("324" "329" "332" "333" "353" "477" "MODE"
                                "JOIN" "PART" "QUIT" "NICK"))
;; (setq erc-complete-functions '(erc-pcomplete erc-button-next))

(setq erc-lurker-threshold-time 3600
      erc-join-buffer 'bury
      ;; erc-autojoin-timing 'ident
      erc-flood-protect nil
      ;; erc-server-send-ping-interval 45
      ;; erc-server-send-ping-timeout 180
      ;; erc-server-reconnect-timeout 60
      erc-server-flood-penalty 1000000
      ;; erc-autojoin-channels-alist '(("freenode.net" "#emacs"))
      ;; erc-prompt-for-nickserv-password nil
      erc-accidental-paste-threshold-seconds 0.5)

;; (setq erc-modules '(netsplit
;;                     fill
;;                     button
;;                     match
;;                     track
;;                     completion
;;                     readonly
;;                     networks
;;                     ring
;;                     autojoin
;;                     noncommands
;;                     irccontrols
;;                     move-to-prompt
;;                     stamp
;;                     menu
;;                     list
;;                     truncate))
;; (erc-update-modules)

;; (delete 'erc-fool-face 'erc-track-faces-priority-list)
;; (delete '(erc-nick-default-face erc-fool-face) 'erc-track-faces-priority-list)

;; (eval-after-load 'erc
;;   '(progn
;;      (define-key erc-mode-map (kbd "C-c r") 'mhc-reset-erc-track-mode)
;;      (define-key erc-mode-map (kbd "C-c C-M-SPC") 'erc-track-clear)
;;      (define-key erc-mode-map (kbd "C-u RET") 'browse-last-url-in-brower)))

;; (defun erc-track-clear ()
;;   (interactive)
;;   (setq erc-modified-channels-alist nil))

;; (defun browse-last-url-in-brower ()
;;   (interactive)
;;   (require 'ffap)
;;   (save-excursion
;;     (let ((ffap-url-regexp "\\(https?://\\)."))
;;       (ffap-next-url t t))))

;; (defun mhc-reset-erc-track-mode ()
;;   (interactive)
;;   (setq erc-modified-channels-alist nil)
;;   (erc-modified-channels-update)
;;   (erc-modified-channels-display))



;; circe
;; (package-require 'circe)
;; (require 'circe)

;; (setq circe-network-options
;;       `(("Freenode"
;;          :nick "mhcat"
;;          :channels ("#clojure" "#emacs")
;;          :nickserv-password ,freenode-password)
;;         ("GIMPNet"
;;          :host "irc.gimp.ca"
;;          :port 6697
;;          :nick "j0ni"
;;          :channels ("#dnalounge")
;;          :nickserv-password ,gimpnet-password
;;          :use-tls t)
;;         ("Gitter"
;;          :host "irc.gitter.im"
;;          :port 6697
;;          :nick "j0ni"
;;          :pass ,gitter-token
;;          :use-tls t)
;;         ("Cloudfront"
;;          :host "irc.cloudfront.net"
;;          :port 7778
;;          :nick "j0ni"
;;          :channels ()
;;          :nickserv-password ,cloudfront-password)
;;         ("OFTC"
;;          :host "irc.oftc.net"
;;          :port 6697
;;          :nick "j0ni"
;;          :channels ("#torontocrypto" "#cryptoparty")
;;          :nickserv-password ,oftc-password
;;          :use-tls t)
;;         ("Mozilla"
;;          :host "irc.mozilla.org"
;;          :port 6697
;;          :use-tls t
;;          :nick "mhcat"
;;          :channels ("#rust" "#rust-beginners"))
;;         ("Mozilla-local"
;;          :host "localhost"
;;          :port 8001
;;          :nick "mhcat"
;;          :pass "lauren4T")
;;         ("Freenode-local"
;;          :host "localhost"
;;          :port 8000
;;          :nick "mhcat"
;;          :pass "lauren4T")))

;; (require 'circe-color-nicks)
;; (enable-circe-color-nicks)

;; (setq tracking-ignored-buffers '(("#clojure-emacs" circe-highlight-nick-face)
;;                                  ("#clojure" circe-highlight-nick-face)
;;                                  ("#lisp" circe-highlight-nick-face)
;;                                  ("#emacs" circe-highlight-nick-face)
;;                                  ("#rust" circe-highlight-nick-face)
;;                                  ("#rust-beginners" circe-highlight-nick-face)))

;; (setq circe-reduce-lurker-spam t)

;; (setq lui-time-stamp-position 'right-margin
;;       lui-fill-type nil)

;; (add-hook 'lui-mode-hook 'my-lui-setup)
;; (defun my-lui-setup ()
;;   (setq fringes-outside-margins t
;;         right-margin-width 7
;;         word-wrap t
;;         wrap-prefix "    "))

;; (defun circe-network-connected-p (network)
;;   "Return non-nil if there's any Circe server-buffer whose
;; `circe-server-netwok' is NETWORK."
;;   (catch 'return
;;     (dolist (buffer (circe-server-buffers))
;;       (with-current-buffer buffer
;;         (if (string= network circe-server-network)
;;             (throw 'return t))))))

;; (defun circe-maybe-connect (network)
;;   "Connect to NETWORK, but ask user for confirmation if it's
;; already been connected to."
;;   (interactive "sNetwork: ")
;;   (if (or (not (circe-network-connected-p network))
;;           (y-or-n-p (format "Already connected to %s, reconnect?" network)))
;;       (circe network)))

;; (defun start-irc ()
;;   "Connect to IRC"
;;   (interactive)
;;   (circe-maybe-connect "Freenode")
;;   (circe-maybe-connect "GIMPNet")
;;   (circe-maybe-connect "Mozilla"))

;; (defun my-circe-message-option-chanserv (nick user host command args)
;;   (when (and (string= "ChanServ" nick)
;;              (string-match "^\\[#.+?\\]" (cadr args)))
;;     '((dont-display . t))))

;; (add-hook 'circe-message-option-functions 'my-circe-message-option-chanserv)

;; (defun my-circe-chanserv-message-handler (nick user host command args)
;;   (when (and (string= "ChanServ" nick)
;;              (string-match "^\\[\\(#.+?\\)\\]" (cadr args)))
;;     (let* ((channel (match-string 1 (cadr args)))
;;            (buffer (circe-server-get-chat-buffer channel t)))
;;       (let ((circe-server-last-active-buffer buffer))
;;         (circe-display-NOTICE nick user host command args)))))

;; (circe-add-message-handler "NOTICE" 'my-circe-chanserv-message-handler)

(provide 'j0ni-irc)
