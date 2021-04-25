(setq gc-cons-threshold (* 100 1024 1024))
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
(setq gc-cons-threshold (* 2 1024 1024))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(go-mode org-bullets-mode elisp--witness--lisp org-bullets markdown-mode exwm xelb web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
