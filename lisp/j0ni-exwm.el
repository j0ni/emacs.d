;;; j0ni-exwm.el --- Setup for EXWM!

;; Taken from exwm and modified

(defun j0ni-exwm-setup ()
  "Default configuration of EXWM."
  (interactive)
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title)))))

(defun exwm-config-misc ()
  "Other configurations."
  ;; Make more room
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; (fringe-mode 1)
  )

(use-package exwm
  :config
  (j0ni-exwm-setup)
  ;; (exwm-systemtray-enable)
  :init
  (require 'exwm-config)
  ;; (require 'exwm-systemtray)
  (require 'exwm-cm)
  ;; Set the initial workspace number.
  (setq exwm-workspace-number 1)
  (setq exwm-input-global-keys
        `(
          ;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-r] . exwm-reset)
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-w] . exwm-workspace-switch)
          ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ;; Bind "s-&" to launch applications ('M-&' also works if the output
          ;; buffer does not bother you).
          ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))

          ([?\s-l] . (lambda ()
                       (interactive)
                       (start-process-shell-command "Lock" nil "/usr/bin/i3lock -e -n -c ff5555")))
          ([?\s-d] . (lambda ()
                       (interactive)
                       (start-process-shell-command "Rofi" nil "/usr/bin/rofi -show combi -dpi 144")))))
  (setq exwm-input-simulation-keys
        '(
          ;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ;; cut/paste.
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])))
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))

  ;; (exwm-enable)

  (exwm-config-ido)
  (exwm-config-misc)
  )


(provide 'j0ni-exwm)
