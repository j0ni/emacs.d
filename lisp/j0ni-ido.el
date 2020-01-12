;;; j0ni-ido.el

;; Enable
(ido-mode t)
(ido-everywhere t)

(setq ido-enable-prefix nil
      ido-case-fold t
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-max-prospects 20
      ;; if it looks promising, use it
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ;; Show previously opened buffers in ido-switch-buffer
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2)

;; Make sure ido is really everywhere
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

;; flx-ido
(use-package flx-ido
  :config
  (flx-ido-mode t)

  :init
  (setq ido-use-faces t)
  (setq flx-ido-use-faces nil))

(use-package flex-isearch
  :config (global-flex-isearch-mode t))

;; Use smex to provide ido-like interface for M-x
(use-package smex
  :config
  (smex-initialize)

  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))

  :init
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Vertical ido
;; (use-package ido-vertical-mode
;;   ;; :config
;;   ;; (ido-vertical-mode)
;;   :init
;;   (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(require 'imenu)
(set-default 'imenu-auto-rescan t)

(use-package idomenu
  :bind
  (("C-t" . idomenu)))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(recentf-mode 1)
(setq recentf-max-menu-items 100
      default-directory (getenv "HOME"))
(global-set-key (kbd "C-x C-M-f") 'recentf-ido-find-file)

(defun ido-my-setup-hook ()
  "Bind `~` to go to homedir when in ido-find-file;
  http://whattheemacsd.com/setup-ido.el-02.html. Also, make space
  less broken."
  ;; (define-key ido-file-completion-map (kbd "~")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (if (looking-back "/")
  ;;         (insert "~/")
  ;;       (call-interactively 'self-insert-command))))
  ;; (define-key ido-completion-map " " 'ido-next-match)
  ;; (define-key ido-completion-map (kbd "C-j") 'ido-exit-minibuffer)
  (define-key ido-file-dir-completion-map (kbd "C-l") 'ido-up-directory))

(add-hook 'ido-setup-hook 'ido-my-setup-hook)

;; There are some parts of ivy we actually like
(use-package swiper
  :init (use-package ivy)
  :bind (("C-s" . swiper))
  :config
  ;; https://github.com/abo-abo/swiper/issues/419
  (advice-add 'swiper--action :after (lambda (throwaway-arg)
                                       (recenter))))

(provide 'j0ni-ido)
