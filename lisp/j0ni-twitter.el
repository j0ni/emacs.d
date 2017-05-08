;;; j0ni-twitter.el --- twitter setup

(package-require 'twittering-mode)

(setq twittering-use-master-password t)

(setq twittering-icon-mode nil)         ; Show icons

(setq twittering-timer-interval 300)    ; Update your timeline each 300
                                        ; seconds (5 minutes)

(setq twittering-url-show-status nil)   ; Keeps the echo area from showing all
                                        ; the http processes

(setq twittering-status-format
      "%RT{%FACE[bold]{RT}}%i %s/%S,  %@:\n%FOLD[  ]{%T // from %f%L%r%R%QT{\n+----\n%FOLD[|]{%i %s/%S,  %@:\n%FOLD[  ]{%T // from %f%L%r%R}}\n+----}}\n ")

(provide 'j0ni-twitter)
