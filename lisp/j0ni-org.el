;;; j0ni-org.el

(package-require 'org)

;; Set to the location of your Org files on your local system
;; (setq org-directory "~/.org")
(setq org-directory j0ni-org-dir)

;; org-journal
(setq org-journal-dir j0ni-org-journal-dir)

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (concat j0ni-org-dir "/flagged.org"))

;; Set to <your Dropbox root directory>/MobileOrg.
;; (setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-directory j0ni-org-dropbox)

;; Set agenda file(s)
(setq org-agenda-files (list (concat j0ni-org-dir "/notebook.org")))

;; track TODO completion
(setq org-log-done 'time)

;; indentation for org-mode
(setq org-startup-indented t)

;; prevent org-mode hijacking arrow keys
(setq org-replace-disputed-keys t)

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)
   (python . t)
   (ruby . t)
   (sh . t)
   (clojure . t)
   (js . t)
   (lisp . t)
   (sql . t)
   (haskell . t)
   (scheme . t)))

;; extra indentation
(setq org-adapt-indentation t)

;; Use cider as the clojure execution backend
(setq org-babel-clojure-backend 'cider)

;; Let's have pretty source code blocks
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

;; hook for clojure programming
(add-hook 'clojure-mode-hook (lambda () (require 'ob-clojure)))

;; Key-bindings for some global commands
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; org-fstree
(package-require 'org-fstree)
(require 'org-fstree)

;; deft
(package-require 'deft)
(setq deft-extension "org"
      deft-directory j0ni-org-dir
      deft-text-mode 'org-mode
      deft-use-filename-as-title t
      deft-auto-save-interval 0)

;; journal
(package-require 'org-journal)

;; bullets
;; (package-require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(provide 'j0ni-org)
