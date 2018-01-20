;;; j0ni-exwm.el --- Setup for EXWM!

;; Taken from exwm and modified

(defun j0ni-exwm-setup ()
  "Default configuration of EXWM."
  ;; Set the initial workspace number.
  (setq exwm-workspace-number 1)
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; 's-r': Reset
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  ;; 's-N': Switch to certain workspace
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
  ;; 's-&': Launch application
  (exwm-input-set-key (kbd "s-SPC")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))
  ;; Line-editing shortcuts
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))))
  ;; Enable EXWM
  (exwm-enable)
  ;; Other configurations
  (exwm-config-misc))

(defun exwm-config-misc ()
  "Other configurations."
  ;; Make more room
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 1))

(use-package exwm
  :ensure t

  :config
  (require 'exwm-config)
  (require 'exwm-systemtray)
  (require 'exwm-cm)
  (j0ni-exwm-setup)
  (exwm-systemtray-enable))

(provide 'j0ni-exwm)
