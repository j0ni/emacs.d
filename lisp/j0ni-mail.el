;;; j0ni-mail.el

(defvar mu-path "/usr/share/emacs/site-lisp/mu4e")

(when (file-exists-p mu-path)
  (add-to-list 'load-path mu-path)
  (require 'mu4e)
  ;; (require 'smtpmail)

  ;; use it everywhere to send mail
  (setq mail-user-agent 'mu4e-user-agent)

  ;; universal
  (setq mu4e-maildir "~/Maildir")

  ;; See http://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html
  ;; for the templates - doco copied into comments below:

  ;; Using mu4e with multiple email accounts is fairly easy. Although
  ;; variables such as user-mail-address, mu4e-sent-folder, message-*,
  ;; smtpmail-*, etc. typically only take one value, it is easy to
  ;; change their values using mu4e-compose-pre-hook. The setup
  ;; described here is one way of doing this (though certainly not the
  ;; only way).

  ;; This setup assumes that you have multiple mail accounts under
  ;; mu4e-maildir. As an example, we’ll use ~/Maildir/Account1 and
  ;; ~/Maildir/Account2, but the setup works just as well if
  ;; mu4e-maildir points to something else.

  ;; First, you need to make sure that all variables that you wish to
  ;; change based on user account are set to some initial value. So set
  ;; up your environment with e.g., your main account:

  ;; (setq mu4e-sent-folder "/Account1/Saved Items"
  ;;       mu4e-drafts-folder "/Account1/Drafts"
  ;;       user-mail-address "my.address@account1.tld"
  ;;       smtpmail-default-smtp-server "smtp.account1.tld"
  ;;       smtpmail-local-domain "account1.tld"
  ;;       smtpmail-smtp-server "smtp.account1.tld"
  ;;       smtpmail-stream-type starttls
  ;;       smtpmail-smtp-service 25)

  ;; default
  (setq mu4e-sent-folder "/Gmail/sent-mail"
        mu4e-auto-retrieve-keys t
        mu4e-decryption-policy t
        mu4e-drafts-folder "/Gmail/drafts"
        mu4e-trash-folder "/Gmail/trash"
        mu4e-update-interval 300
        mu4e-confirm-quit nil
        mu4e-use-fancy-chars nil ;; they actually look shit
        ;; mu4e-html2text-command "pandoc -f html -t markdown"
        ;; mu4e-html2text-command "w3m -dump -T text/html"
        ;; mu4e-html2text-command "html2text -utf8 -width 72"
        ;; mu4e-html2text-command 'mu4e-shr2text
        shr-color-visible-luminance-min 70
        mu4e-headers-sort-direction 'ascending
        mu4e-headers-skip-duplicates t
        mu4e-headers-hide-predicate nil
        mu4e-headers-include-related nil
        mu4e-split-view 'single-window
        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:mailing-list . 10)
                              (:from . 22)
                              (:thread-subject))
        mu4e-compose-complete-only-after "2012-01-01"
        mu4e-view-show-addresses t
        mu4e-date-format-long "%FT%T%z")

  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text)

  ;; something about ourselves
  (setq mu4e-user-mail-address-list '("j@lollyshouse.ca"
                                      "jonathan.irving@gmail.com"
                                      "j@appcanary.com"
                                      "jonathan@appcanary.com"
                                      "j0ni@appcanary.com"
                                      "jon@motiva.ai")
        user-full-name "J Irving"
        mu4e-compose-signature "Jonathan Irving\nhttps://j0ni.ca")

  (setq mu4e-bookmarks
        '(("date:24h..now AND NOT flag:trashed" "Last day's messages" ?t)
          ("date:24h..now" "Last day's messages (with trash)" ?T)
          ("date:7d..now AND NOT flag:trashed" "Last 7 days" ?w)
          ("date:1y..now AND NOT flag:trashed" "Last year" ?Y)
          ("(maildir:/Gmail/INBOX OR maildir:/Gmail/sent-mail) AND date:1y..now AND NOT flag:trashed" "Last year (Gmail)" ?y)
          ("date:7d..now" "Last 7 days (with trash)" ?W)
          ("mime:image/*" "Messages with images" ?i)

          ("date:3y..now AND (maildir:/Circle/INBOX OR maildir:/Circle/sent-mail) AND NOT flag:trashed"
           "Last 3 years (Circle)"
           ?C)

          ("date:24h..now AND (maildir:/Gmail/INBOX OR maildir:/Gmail/sent-mail) AND NOT flag:trashed"
           "Last day's messages (Gmail)"
           ?g)

          ("date:7d..now AND (maildir:/Gmail/INBOX OR maildir:/Gmail/sent-mail) AND NOT flag:trashed"
           "Last 7 days (Gmail)"
           ?G)

          ("date:7d..now AND maildir:/Gmail/Clojure AND NOT flag:trashed"
           "Last 7 days (Clojure)"
           ?c)

          ;; ("date:today..now AND NOT flag:trashed AND (maildir:/Appcanary/INBOX OR maildir:/Appcanary/sent-mail)"
          ;;  "Today's messages (Appcanary)"
          ;;  ?a)

          ;; ("date:7d..now AND NOT flag:trashed AND (maildir:/Appcanary/INBOX OR maildir:/Appcanary/sent-mail)"
          ;;  "Last 7 days (Appcanary)"
          ;;  ?A)

          ;; ("date:today..now AND NOT flag:trashed AND (maildir:/Motiva/INBOX OR maildir:/Motiva/sent-mail)"
          ;;  "Today's messages (Motiva)"
          ;;  ?m)

          ;; ("date:7d..now AND NOT flag:trashed AND (maildir:/Motiva/INBOX OR maildir:/Motiva/sent-mail)"
          ;;  "Last 7 days (Motiva)"
          ;;  ?M)

          ))

  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
        message-sendmail-envelope-from 'header)

  ;; Borrowed and tweaked from http://zmalltalker.com/linux/mu.html:

  ;; Borrowed from http://ionrock.org/emacs-email-and-mu.html
  ;; Choose account label to feed msmtp -a option based on From header
  ;; in Message buffer; This function must be added to
  ;; message-send-mail-hook for on-the-fly change of From address before
  ;; sending message since message-send-mail-hook is processed right
  ;; before sending message.

  (defun choose-msmtp-account ()
    (if (message-mail-p)
        (save-excursion
          (let*
              ((from (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "from")))
               (account
                (cond
                 ((string-match "j@appcanary.com" from) "appcanary")
                 ((string-match "j@lollyshouse.ca" from) "gmail")
                 ((string-match "jon@motiva.ai" from) "motiva"))))
            (setq message-sendmail-extra-arguments (list '"-a" account))))))

  (add-hook 'message-send-mail-hook 'choose-msmtp-account)

  ;; (setq message-send-mail-function 'smtpmail-send-it
  ;;       smtpmail-stream-type 'starttls
  ;;       smtpmail-default-smtp-server "smtp.gmail.com"
  ;;       smtpmail-smtp-server "smtp.gmail.com"
  ;;       smtpmail-smtp-service 587)

  ;; account management

  ;; Then create a variable my-mu4e-account-alist, which should contain
  ;; a list for each of your accounts. Each list should start with the
  ;; account name, (which must be identical to the account’s directory
  ;; name under ~/Maildir), followed by (variable value) pairs:

  (defvar my-mu4e-account-alist
    `(("Gmail"
       (mu4e-sent-folder "/Gmail/sent-mail")
       (mu4e-drafts-folder "/Gmail/drafts")
       (mu4e-maildir-shortcuts (("/Gmail/INBOX"     . ?i)
                                ("/Gmail/all-mail"  . ?a)
                                ("/Gmail/sent-mail" . ?s)
                                ("/Gmail/drafts"    . ?d)
                                ("/Gmail/trash"     . ?t)))
       (user-mail-address "j@lollyshouse.ca")
       (mu4e-compose-signature "Jonathan Irving\nhttps://j0ni.ca"))
      ;; ("Appcanary"
      ;;  (mu4e-sent-folder "/Appcanary/sent-mail")
      ;;  (mu4e-drafts-folder "/Appcanary/drafts")
      ;;  (mu4e-maildir-shortcuts (("/Appcanary/INBOX"     . ?i)
      ;;                           ("/Appcanary/all-mail"  . ?a)
      ;;                           ("/Appcanary/sent-mail" . ?s)
      ;;                           ("/Appcanary/drafts"    . ?d)
      ;;                           ("/Appcanary/trash"     . ?t)))
      ;;  (user-mail-address "j@appcanary.com")
      ;;  (mu4e-compose-signature "Jonathan Irving\nhttps://appcanary.com\nhttps://j0ni.ca"))
      ;; ("Motiva"
      ;;  (mu4e-sent-folder "/Motiva/sent-mail")
      ;;  (mu4e-drafts-folder "/Motiva/drafts")
      ;;  (mu4e-maildir-shortcuts (("/Motiva/INBOX"     . ?i)
      ;;                           ("/Motiva/all-mail"  . ?a)
      ;;                           ("/Motiva/sent-mail" . ?s)
      ;;                           ("/Motiva/drafts"    . ?d)
      ;;                           ("/Motiva/trash"     . ?t)))
      ;;  (user-mail-address "jon@motiva.ai")
      ;;  (mu4e-compose-signature "Jonathan Irving\nhttp://motiva.ai\nhttps://j0ni.ca"))
      ))

  ;; You can put any variable you want in the account lists, just make
  ;; sure that you put in all the variables that differ for each
  ;; account. Variables that do not differ need not be included. For
  ;; example, if you use the same smtp server for both accounts, you
  ;; don’t need to include the smtp-related variables in
  ;; my-mu4e-account-alist.

  ;; Note that some SMTP servers (such as Gmail) require the SMTP
  ;; username to match the user mail address. In this case your mail
  ;; will appear to originate from whichever SMTP account you use. Thus
  ;; unless you are certain your SMTP server does not have this
  ;; requirement, you should generally use different SMTP account
  ;; credentials for each mail account.

  ;; Now, the following function can be used to select an account and
  ;; set the variables in my-mu4e-account-alist to the correct values:

  (defun my-mu4e-set-account ()
    "Set the account for composing a message."
    (let* ((account
            (if mu4e-compose-parent-message
                (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                  (string-match "/\\(.*?\\)/" maildir)
                  (match-string 1 maildir))
              (completing-read (format "Compose with account: (%s) "
                                       (mapconcat #'(lambda (var) (car var))
                                                  my-mu4e-account-alist "/"))
                               (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                               nil t nil nil (caar my-mu4e-account-alist))))
           (account-vars (cdr (assoc account my-mu4e-account-alist))))
      (if account-vars
          (mapc #'(lambda (var)
                    (set (car var) (cadr var)))
                account-vars)
        (error "No email account found"))))

  ;; This function then needs to be added to mu4e-compose-pre-hook:
  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

  ;; This way, my-mu4e-set-account will be called every time you edit a message.
  ;; If you compose a new message, it simply asks you for the account you wish
  ;; to send the message from (TAB completion works). If you’re replying or
  ;; forwarding a message, or editing an existing draft, the account is chosen
  ;; automatically, based on the first component of the maildir of the message
  ;; being replied to, forwarded or edited (i.e., the directory under
  ;; ~/Maildir).

  ;; end of account management stuff

  ;; Don't save message to Sent Messages, Gmail/IMAP takes care of this (See the
  ;; documentation for `mu4e-sent-messages-behavior' if you have additional
  ;; non-Gmail addresses and want assign them different behavior.)
  (setq mu4e-sent-messages-behavior 'delete)


  ;; From http://zmalltalker.com/linux/mu.html again:

  ;; Wouldn't it be awesome to be able to send files from dired using your mail
  ;; client?

  ;; I'll need a special version of the gnus-dired-mail-buffers function so it
  ;; understands mu4e buffers as well:

  (require 'gnus-dired)

  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode

  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  ;; With this, I can attach a file as an attachment to a new email
  ;; message by entering C-c RET C-a, and I'm good to go.

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Set up compose mode

  (defun my-compose-mode-setup ()
    "My settings for message composition."
    (set-fill-column 72)
    (turn-on-auto-fill)
    ;; (flyspell-mode)
    (mml-secure-message-sign-pgpmime))

  (add-hook 'mu4e-compose-mode-hook 'my-compose-mode-setup)
  (add-hook 'message-mode-hook 'my-compose-mode-setup)

  ;; Citation
  (package-require 'mu-cite)
  (setq mail-citation-hook 'mu-cite-original)
  (setq message-citation-hook 'mu-cite-original)
  (setq mu-cite-top-format '(">>>>>> " from " at " date ":\n\n"))
  (setq mu-cite-prefix-format '("  > "))

  ;; Eliding
  (setq message-elide-ellipsis "[... elided ...]")

  ;; (setq message-citation-line-format "* %f, on %Y-%m-%d @ %R %z:")
  ;; (setq message-citation-line-function 'message-insert-formatted-citation-line)

  (define-key mu4e-compose-mode-map (kbd "C-c C-,") 'sign-off-email)
  (define-key mu4e-compose-mode-map (kbd "C-c ,") 'sign-off-email)

  (defun my-key-bindings ()
    (define-key mail-mode-map [(control c) (control c)]
      (lambda ()
        "Save buffer and send email"
        (interactive)
        (save-buffer)
        (server-edit)))))

;; Mutt config

(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook 'mail-mode-hook 'my-key-bindings)

(defun sign-off-email ()
  (interactive)
  (insert "   cheers, J\n   ")
  ;; (insert (format-time-string "%Y-%m-%d @ %H:%M:%S %z\n"))
  )

(add-to-list 'auto-mode-alist '("mutt-ship-.*$" . message-mode))

(provide 'j0ni-mail)
