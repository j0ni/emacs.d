;;; j0ni-esk.el --- emacs starter kit bits

;; This stuff is grafted directly from the ESK components I was using and
;; slightly edited. At some point I will go through this and prune it more, but
;; for now I'm leaving it in place as is, because I don't have the time or the
;; inclination to figure it out and trim it down.

;; Turns out I'm really tweaking this a bit here and there for the time being.

(progn
  ;; Turn off mouse interface early in startup to avoid momentary display
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

  ;; from ESK's defuns

  (defun esk-local-column-number-mode ()
    (make-local-variable 'column-number-mode)
    (column-number-mode t))

  (defun esk-local-comment-auto-fill ()
    (set (make-local-variable 'comment-auto-fill-only-comments) t)
    (auto-fill-mode t))

  (defun esk-turn-on-save-place-mode ()
    (require 'saveplace)
    (setq save-place t))

  (defun esk-add-watchwords ()
    (font-lock-add-keywords
     nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
            1 font-lock-warning-face t))))

  (add-hook 'prog-mode-hook 'esk-local-column-number-mode)
  (add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
  (add-hook 'prog-mode-hook 'esk-turn-on-save-place-mode)
  (add-hook 'prog-mode-hook 'esk-add-watchwords)

  (defun esk-prog-mode-hook ()
    (run-hooks 'prog-mode-hook))

  (defun esk-turn-off-tool-bar ()
    (if (functionp 'tool-bar-mode) (tool-bar-mode -1)))

  (defun esk-indent-buffer ()
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun esk-cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a buffer."
    (interactive)
    (esk-indent-buffer)
    (esk-untabify-buffer)
    (delete-trailing-whitespace))

  ;; Commands

  (defun esk-eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))

  (defun esk-sudo-edit (&optional arg)
    (interactive "p")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  (defun esk-insert-date ()
    "Insert a time-stamp according to locale's date and time format."
    (interactive)
    (insert (format-time-string "%c" (current-time))))

  (defun esk-paredit-nonlisp ()
    "Turn on paredit mode for non-lisps."
    (interactive)
    (set (make-local-variable 'paredit-space-for-delimiter-predicates)
         '((lambda (endp delimiter) nil)))
    (paredit-mode 1))

  ;; A monkeypatch to cause annotate to ignore whitespace
  (defun vc-git-annotate-command (file buf &optional rev)
    (let ((name (file-relative-name file)))
      (vc-git-command buf 0 name "blame" "-w" rev)))

  ;; end defuns

  ;; starter kit misc

  (when window-system
    (setq frame-title-format '(buffer-file-name "%f" ("%b")))
    (tooltip-mode -1)
    (mouse-wheel-mode t)
    (blink-cursor-mode -1))

  ;; can't do it at launch or emacsclient won't always honor it
  (add-hook 'before-make-frame-hook 'esk-turn-off-tool-bar)

  (setq visible-bell t
        inhibit-startup-message t
        color-theme-is-global t
        shift-select-mode nil
        mouse-yank-at-point t
        uniquify-buffer-name-style 'forward
        ediff-window-setup-function 'ediff-setup-windows-plain
        save-place-file (concat user-emacs-directory "places")
        backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
        diff-switches "-u")

  (add-to-list 'safe-local-variable-values '(lexical-binding . t))
  (add-to-list 'safe-local-variable-values '(whitespace-line-column . 100))

  ;; Set this to whatever browser you use
  ;; (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-browser-function 'browse-url-default-browser)
  ;; (setq browse-url-browser-function 'browse-url-firefox)
  ;; (setq browse-url-browser-function 'browse-default-macosx-browser)
  ;; (setq browse-url-browser-function 'browse-default-windows-browser)
  ;; (setq browse-url-browser-function 'browse-default-kde)
  ;; (setq browse-url-browser-function 'browse-default-epiphany)
  ;; (setq browse-url-browser-function 'browse-default-w3m)
  ;; (setq browse-url-browser-function 'browse-url-generic
  ;;       browse-url-generic-program "~/src/conkeror/conkeror")

  ;; Highlight matching parentheses when the point is on them.
  (show-paren-mode 1)

  (require 'ffap)
  (defvar ffap-c-commment-regexp "^/\\*+"
    "Matches an opening C-style comment, like \"/***\".")

  (defadvice ffap-file-at-point (after avoid-c-comments activate)
    "Don't return paths like \"/******\" unless they actually exist.

This fixes the bug where ido would try to suggest a C-style
comment as a filename."
    (ignore-errors
      (when (and ad-return-value
                 (string-match-p ffap-c-commment-regexp
                                 ad-return-value)
                 (not (ffap-file-exists-string ad-return-value)))
        (setq ad-return-value nil))))

  (set-default 'indicate-empty-lines t)
  (set-default 'imenu-auto-rescan t)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'auto-tail-revert-mode 'tail-mode)

  (random t) ;; Seed the random-number generator

  ;; Hippie expand: at times perhaps too hip
  (eval-after-load 'hippie-exp
    '(progn
       (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
         (delete f hippie-expand-try-functions-list))

       ;; Add this back in at the end of the list.
       (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)))

  (eval-after-load 'grep
    '(when (boundp 'grep-find-ignored-files)
       (add-to-list 'grep-find-ignored-files "*.class"))))

;; end misc

;; some starter kit bindings
(progn
  ;; Jump to a definition in the current file. (Protip: this is awesome.)
  (global-set-key (kbd "C-x C-i") 'imenu)

  ;; File finding
  (global-set-key (kbd "C-c y") 'bury-buffer)
  (global-set-key (kbd "C-c r") 'revert-buffer)

  ;; Window switching. (C-x o goes to the next window)
  (windmove-default-keybindings) ;; Shift+direction
  (global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
  (global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

  ;; If you want to be able to M-x without meta (phones, etc)
  (global-set-key (kbd "C-c x") 'execute-extended-command)

  ;; Help should search more than just commands
  (define-key 'help-command "a" 'apropos)

  ;; M-S-6 is awkward
  (global-set-key (kbd "C-c q") 'join-line)

  ;; This is a little hacky since VC doesn't support git add internally
  (eval-after-load 'vc
    '(define-key vc-prefix-map "i"
       (lambda () (interactive)
         (if (not (eq 'Git (vc-backend buffer-file-name)))
             (vc-register)
           (shell-command (format "git add %s" buffer-file-name))
           (message "Staged changes.")))))

  ;; activate occur easily inside isearch
  (define-key isearch-mode-map (kbd "C-o")
    (lambda () (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))))

;; end bindings

(provide 'j0ni-esk)
