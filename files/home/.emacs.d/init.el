(setq gc-cons-threshold (* 20 1000 1000))
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(exwm web-mode elisp--witness--lisp))
 '(safe-local-variable-values
   '((diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (bidi-paragraph-direction . right-to-left)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
