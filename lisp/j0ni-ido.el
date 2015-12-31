;;; j0ni-ido.el

;; Enable
(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-prefix nil
      ido-case-fold t
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-max-prospects 100
      ;; if it looks promising, use it
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ;; Show previously opened buffers in ido-switch-buffer
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2)

;; Make sure ido is really everywhere
(package-require 'ido-ubiquitous)
(ido-ubiquitous-mode)

;; flx-ido
(package-require 'flx-ido)
(flx-ido-mode)
(setq ido-use-faces nil
      flx-ido-use-faces t)

;; Use smex to provide ido-like interface for M-x
(package-require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))

;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Vertical ido
(package-require 'ido-vertical-mode)
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; ido-imenu
(require 'imenu)
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (cl-flet ((addsymbols (symbol-list)
                          (when (listp symbol-list)
                            (dolist (symbol symbol-list)
                              (let ((name nil) (position nil))
                                (cond
                                 ((and (listp symbol) (imenu--subalist-p symbol))
                                  (addsymbols symbol))

                                 ((listp symbol)
                                  (setq name (car symbol))
                                  (setq position (cdr symbol)))

                                 ((stringp symbol)
                                  (setq name symbol)
                                  (setq position (get-text-property 1 'org-imenu-marker symbol))))

                                (unless (or (null position) (null name))
                                  (add-to-list 'symbol-names name)
                                  (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(set-default 'imenu-auto-rescan t)

(package-require 'idomenu)
(global-set-key (kbd "C-t") 'idomenu)

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
  ;; "Bind `~` to go to homedir when in ido-find-file;
  ;; http://whattheemacsd.com/setup-ido.el-02.html. Also, make space
  ;; less broken."
  ;; (define-key ido-file-completion-map (kbd "~")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (if (looking-back "/")
  ;;         (insert "~/")
  ;;       (call-interactively 'self-insert-command))))
  (define-key ido-completion-map " " 'ido-next-match)
  (define-key ido-completion-map (kbd "C-j") 'ido-exit-minibuffer)
  (define-key ido-file-dir-completion-map (kbd "C-l") 'ido-up-directory))

(add-hook 'ido-setup-hook 'ido-my-setup-hook)

(provide 'j0ni-ido)
