;;; j0ni-org.el

(package-require 'org)

;; Set to the location of your Org files on your local system
(setq org-directory j0ni-org-dir)

;; org-journal
(setq org-journal-dir (concat j0ni-org-dir "Journal/"))
(setq org-journal-find-file 'find-file)

;; ensure the journal files get picked up
(setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (concat j0ni-org-dir "flagged.org"))

;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory j0ni-org-dropbox)

;; track TODO completion
(setq org-log-done 'time)

;; get a completion note - let's see how easy this is to deal with
(setq org-log-done 'note)

;; time tracking
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; indentation for org-mode
(setq org-startup-indented t)

(with-eval-after-load 'org
  '(progn
     ;; Set agenda file(s)
     (setq org-agenda-files (list org-journal-dir
                                  (concat j0ni-org-dir "Agenda/")))

     (dolist (tag '(circle skalera orchard home appcanary motiva sanity))'
       (add-to-list 'org-tag-persistent-alist tag))))

;; prevent org-mode hijacking arrow keys
(setq org-replace-disputed-keys t)

;; switch quickly
(setq org-use-fast-todo-selection t)

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)
   (python . t)
   (ipython . t)
   (ruby . t)
   ;; (sh . t)
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

;; org-capture
(setq org-default-notes-file (concat j0ni-org-dir "captured.org"))
(setq org-capture-templates
      '(("j" "Journal entry" entry (file+datetree "")
         "* %<%H:%M> %?\n")
        ("J" "Hinted Journal entry" entry (file+datetree "")
         "* %<%H:%M> %?\n** How do I feel?\n** What have I been doing?\n** What shall I do next?")
        ("t" "Task" entry (file+headline "" "Tasks")
         "* TODO %?\n%u")))

;; org-fstree
(package-require 'org-fstree)
(require 'org-fstree)

;; deft
(package-require 'deft)
(setq deft-extension "org"
      deft-directory j0ni-org-dir
      deft-recursive t
      deft-text-mode 'org-mode
      deft-use-filename-as-title t
      deft-auto-save-interval 0)

;; journal
(package-require 'org-journal)

;; bullets
;; (package-require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; let's try synchronizing with github
;; (package-require 'org-sync)
;; (require 'org-sync-github)
;; (setq org-sync-github-auth '("j0ni" . "blah"))

(provide 'j0ni-org)
