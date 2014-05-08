;;; init.el --- new init, based on Bodil Stokke's

(defvar j0ni-font "PragmataPro-14")
(defvar j0ni-linum-font "PragmataPro-11")
;; use this to play with new fonts - not defined yet
;; (apply-font-settings)

;; Themes we want to install
(defvar j0ni-installed-themes
  '(soothe-theme
    toxi-theme
    noctilux-theme
    cyberpunk-theme
    subatomic-theme
    ir-black-theme
    twilight-theme
    twilight-bright-theme
    twilight-anti-bright-theme
    zenburn-theme
    bubbleberry-theme
    ;; solarized-theme
    phoenix-dark-pink-theme
    phoenix-dark-mono-theme))

;; Theme I like at the moment
(defvar j0ni-theme 'phoenix-dark-pink)

(defun concat-home (dir)
  (concat (getenv "HOME") "/" dir))

(defvar j0ni-path (list (concat-home ".rbenv/shims")
                        (concat-home ".rbenv/bin")
                        (concat-home ".cabal/bin")
                        "/usr/local/bin"))
(defvar j0ni-git-contrib-dir "/usr/local/share/git-core/contrib/emacs")
(defvar j0ni-org-dir (concat-home "Dropbox/OrgMode"))
(defvar j0ni-notebook (concat j0ni-org-dir "/notebook.org"))
(defvar j0ni-org-dropbox (concat-home "Dropbox/Apps/MobileOrg"))
(defvar j0ni-agenda-files (list j0ni-notebook))
(defvar j0ni-org-journal-dir (concat j0ni-org-dir "/Journal"))
(defvar j0ni-go-path (list (concat-home "Scratch/go")
                           (concat-home "Scratch/goeg")))

;; switch some stuff off
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; and some other stuff on
(dolist (mode '(blink-cursor-mode line-number-mode column-number-mode))
  (when (fboundp mode) (funcall mode +1)))

(setq mac-command-modifier 'meta)

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Automatically save buffers before compiling
(setq compilation-ask-about-save nil)

;; Setup exec-path and PATH environment variable, in case shell has failed to do so
(let ((paths j0ni-path))
  (setenv "PATH" (apply 'concat
                        (append (mapcar (lambda (i) (concat i ":")) paths)
                                (list (getenv "PATH")))))
  (dolist (path paths) (when (file-directory-p path)
                         (add-to-list 'exec-path path))))


;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Get hostname
(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" (with-output-to-string (call-process "hostname" nil standard-output))))

;; Add .emacs.d/lisp to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "lisp"))

;; Set paths to custom.el and loaddefs.el
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file)

;; Add every subdirectory of ~/.emacs.d/site-lisp to the load path
(dolist (project (directory-files (concat dotfiles-dir "site-lisp") t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Detect online status, from ESK
(require 'cl)
(defun esk-online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                              (member 'up (first (last (network-interface-info
                                                        (car iface)))))))
            (network-interface-list))
    t))


;; ELPA
(setq package-user-dir (concat dotfiles-dir "elpa"))
(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(when (esk-online?)
  (unless package-archive-contents (package-refresh-contents)))

(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(defun packages-require (pkg-list)
  "Install a list of packages using package-require."
  (dolist (pkg pkg-list)
    (package-require pkg)))

(setq j0ni-pkg-full
      '(j0ni-defuns
        j0ni-gui
        j0ni-powerline
        j0ni-lisp
        j0ni-flycheck
        j0ni-ido
        j0ni-go
        j0ni-js
        j0ni-git
        j0ni-ruby
        j0ni-markup
        j0ni-haskell
        j0ni-org
        j0ni-python
        j0ni-complete))

;; For now - migrate the useful stuff to my config later
(packages-require
 '(starter-kit-bindings
   starter-kit-lisp
   starter-kit-eshell))

(dolist (file j0ni-pkg-full)
  (require file))







;;;; old stuff



(require 'easymenu)

;; everything to install from elpa repo
(defvar my-other-packages
  '(elisp-slime-nav
    ;; slime
    sicp
    ssh-config-mode
    dropdown-list
    projectile

    adaptive-wrap
    auto-compile
    popup

    company
    company-go
    company-inf-python
    company-inf-ruby
    company-tern
    auto-complete ;; elpy needs this to be present :(
    ;; ac-slime
    ;; go-autocomplete
    ;; ac-js2
    ;; ac-nrepl

    autopair

    find-file-in-project
    dash
    undo-tree
    browse-kill-ring


    markdown-mode
    ;; python
    virtualenv
    py-autopep8
    elpy

    sr-speedbar

    apache-mode
    puppet-mode
    log4j-mode

    highlight-symbol
    highlight-parentheses
    yasnippet
    rainbow-delimiters
    ack-and-a-half
    smart-tab
    csv-mode
    wc-mode
    smooth-scrolling
    diminish

    scala-mode2

    csv-nav
    figlet
    http-twiddle
    paredit-everywhere

))

(dolist (p my-other-packages)
  (when (not (package-installed-p p)) (package-install p)))


;; find-file-in-project
(setq ffip-full-paths t)

;; undo-tree
(global-undo-tree-mode t)
(diminish 'undo-tree-mode)

;; projectile
(projectile-global-mode)
(diminish 'projectile-mode)

;; wc-mode
(require 'wc-mode)

(remove-hook 'prog-mode-hook 'esk-pretty-lambdas)
(remove-hook 'prog-mode-hook 'esk-pretty-functions)
(remove-hook 'prog-mode-hook 'idle-highlight-mode)
(remove-hook 'text-mode-hook 'turn-on-flyspell)

(add-hook 'prog-mode-hook 'linum-mode)

(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

;; (add-hook 'prog-mode-hook 'which-function-mode)

(defun turn-off-auto-fill ()
  (auto-fill-mode -1))

(require 'smartparens-config)
;; (smartparens-global-mode 1)

(defun hippie-unexpand ()
  (interactive)
  (hippie-expand nil))

(define-key read-expression-map [(shift tab)] 'hippie-unexpand)

;; key mappings
(defun map-local-ret ()
  (interactive)
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'prog-mode-hook 'map-local-ret)
(add-hook 'text-mode-hook 'map-local-ret)
(remove-hook 'jade-mode-hook 'map-local-ret)

;; remap dynamic expansion to escape
(global-set-key (kbd "<escape>") 'hippie-expand)

;; map start of file and end of file commands to nicer key combos
(global-set-key (read-kbd-macro "M-[") 'beginning-of-buffer)
(global-set-key (read-kbd-macro "M-]") 'end-of-buffer)

;; disable tab indent
(setq-default indent-tabs-mode nil)

(setq default-tab-width 4)

;; pipe down
(setq bell-volume 0
      sound-alist nil)

;; deal with broken find-file-at-point
(setq ffap-machine-p-known 'reject)
;; (global-hl-line-mode 1)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      ;; scroll-conservatively 0
      scroll-preserve-screen-position 1)

;; (setq smooth-scroll-margin 2)

;; don't eat my shell
(setq-default comint-prompt-read-only t)

;; disable backup
(setq backup-inhibited t)

;; don't auto-save
(setq auto-save-default nil)


;; python
(progn
  (add-hook 'before-save-hook 'py-autopep8-before-save)
  (add-hook 'python-mode-hook 'turn-on-smartparens-mode)
  (elpy-enable)
  (elpy-clean-modeline)
  (delete 'highlight-indentation-mode elpy-default-minor-modes)
  (delete 'auto-complete-mode elpy-default-minor-modes))

;; ack
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; java
(progn
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode)))

(add-hook 'puppet-mode 'turn-on-smartparens-mode)

;; markdown
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(setq markdown-command "redcarpet")

;; yasnippets and autocompletion
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/vendor-snippets/js-yasnippets")
(add-to-list 'yas-snippet-dirs "~/.emacs.d/vendor-snippets/yasnippets")

(require 'dropdown-list)

(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))

(setq hippie-expand-try-functions-list '(yas-hippie-try-expand
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))


;; for mutt
(progn
  (add-to-list 'auto-mode-alist '("/mutt" . message-mode))
  (add-hook 'message-mode-hook (lambda () (load-theme 'phoenix-dark-mono))))

(winner-mode t)

;; tramp
(setq tramp-default-method "ssh")

;; info
(add-to-list 'Info-directory-list "~/Scratch/Lisp/on-lisp/")
(add-to-list 'Info-directory-list "/usr/local/share/info/")

;; clock in the mode-line
(setq display-time-format "%H:%M")
(display-time-mode t)

(define-abbrev-table 'global-abbrev-table
  '(("alpha" "α")
    ("beta" "β")
    ("gamma" "γ")
    ("theta" "θ")
    ("inf" "∞")

    ("ar1" "→")
    ("ar2" "⇒")

    ("pi" "π")))

(abbrev-mode 1)

;; for sr-speedbar in 24.3.90+
;; (defun ad-advised-definition-p (definition)
;;   "Return non-nil if DEFINITION was generated from advice information."
;;    (if (or (ad-lambda-p definition) (macrop definition) (ad-compiled-p definition))
;;        (let ((docstring (ad-docstring definition)))
;;          (and (stringp docstring)
;;               (get-text-property 0 'dynamic-docstring-function docstring)))))

(setq speedbar-use-images nil)

(require 'ws-butler)
(ws-butler-global-mode)
