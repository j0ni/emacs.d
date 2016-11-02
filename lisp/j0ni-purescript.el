;;; j0ni-purescript.el --- purescript setup

(packages-require '(purescript-mode
                    psc-ide
                    psci))

(add-to-list 'auto-mode-alist (cons "\\.purs\\'" 'purescript-mode))

(add-hook 'purescript-mode-hook
          (lambda ()
            (psc-ide-mode)
            (flycheck-mode)
            (turn-on-purescript-indentation)
            (inferior-psci-mode)))

(defun purescript-doc-current-info () nil)

;; Deal with broken psc-ide integration
;; (eval-after-load 'psc-ide
;;   (lambda ()
;;     (defun psc-ide--version-gte (version1 version2)
;;       "Determines whether VERSION1 is greater then or equal to VERSION2"
;;       (let ((vs (-zip-fill 0
;;                            (-map 'string-to-int (s-split "\\." version1))
;;                            (-map 'string-to-int (s-split "\\." version2))))
;;             (comp nil))
;;         (dolist (p vs comp)
;;           (when (null comp)
;;             (setq comp (cond ((< (car p) (cdr p)) 'lt)
;;                              ((> (car p) (cdr p)) 'gt)))))
;;         (not (equal 'lt comp))))))

;; Generate a likely module name from the current file path
(package-require 'f)
(package-require 's)
(require 'f)
(require 's)
(defun purescript-module-name-from-current-buffer-file ()
  (let ((path (f-split (f-relative
                        (f-base (buffer-file-name))
                        (f-join (projectile-project-root) "src"))))
        (testpath (f-split (f-relative
                            (f-base (buffer-file-name))
                            (f-join (projectile-project-root) "test")))))
    (if (string= ".." (car path))
        (if (string= ".." (car testpath)) "Main" (s-join "." (cons "Test" testpath)))
      (s-join "." path))))

(provide 'j0ni-purescript)
