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
      '(j0ni-esk
        j0ni-defuns
        j0ni-gui
        j0ni-misc
        j0ni-powerline
        j0ni-snippets
        j0ni-codestyle
        j0ni-lisp
        j0ni-flycheck
        j0ni-ido
        j0ni-go
        j0ni-js
        j0ni-git
        j0ni-ruby
        j0ni-markup
        j0ni-markdown
        j0ni-haskell
        j0ni-org
        j0ni-python
        j0ni-complete))

(dolist (file j0ni-pkg-full)
  (require file))
