;;; j0ni-esk.el --- emacs starter kit bits

;; For now - migrate the useful stuff to my config later
(packages-require
 '(starter-kit-bindings
   starter-kit-lisp
   starter-kit-eshell))

(remove-hook 'prog-mode-hook 'esk-pretty-lambdas)
(remove-hook 'prog-mode-hook 'esk-pretty-functions)
(remove-hook 'prog-mode-hook 'esk-pretty-fn)
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)
(remove-hook 'prog-mode-hook 'idle-highlight-mode)
(remove-hook 'text-mode-hook 'turn-on-flyspell)

(provide 'j0ni-esk)
