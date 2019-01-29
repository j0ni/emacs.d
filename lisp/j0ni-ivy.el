;;; j0ni-ivy.el

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 15)
  ;; (setq ivy-height 20)
  (setq ivy-length 200)
  ;; (setq ivy-count-format "")
  (setq confirm-nonexistent-file-or-buffer t)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  ;; (setq ivy-format-function (lambda (cands)
  ;;                             (ivy--format-function-generic
  ;;                              (lambda (str)
  ;;                                (ivy--add-face str 'ivy-current-match))
  ;;                              #'identity
  ;;                              cands
  ;;                              " | ")))
  (setq ivy-format-function 'ivy-format-function-default)
  (setq ivy-display-function nil)
  ;; w00t
  (setq ivy-extra-directories nil)

  :config
  (use-package smex)
  (ivy-mode t)

  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("RET" . ivy-done))

  :diminish ivy-mode)

;; (ivy-mode t)
;; (diminish 'ivy-mode)

;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-height 20)
;; (setq ivy-length 200)
;; ;; (setq ivy-count-format "")
;; (setq confirm-nonexistent-file-or-buffer t)


(use-package avy
  :config
  (avy-setup-default)
  :bind
  (("C-c C-j" . avy-resume)
   ("M-g w" . avy-goto-word-1)
   ("C-'" . avy-goto-char-2)))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper))
  :config
  ;; https://github.com/abo-abo/swiper/issues/419
  (advice-add 'swiper--action :after (lambda (throwaway-arg)
                                       (recenter))))


;; (global-set-key (kbd "C-s") 'swiper)

(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)

         ;; Hmmm... I doubt I'll ever use these
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)

         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep) ;; cuz it might be gif :P
         ("C-c a g" . counsel-ag)
         ("C-x l" . counsel-locate)
         ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

         ("C-\\" . counsel-company)))

;; I have no use for dired
;; (define-key ivy-minibuffer-map (kbd "RET") 'ivy-done)

;; set elsewhere
;; (setq magit-completing-read-function 'ivy-completing-read)
;; (setq projectile-completion-system 'ivy)

(use-package counsel-projectile
  :after (:all projectile ivy)
  :init
  (setq projectile-keymap-prefix (kbd "C-c C-p"))
  :config (counsel-projectile-mode))

(use-package imenu-anywhere
  :bind (("C-." . imenu-anywhere)))

;; (use-package ivy-lobsters
;;   :ensure t)

(provide 'j0ni-ivy)
