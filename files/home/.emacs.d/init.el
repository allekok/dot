(setq gc-cons-threshold (* 150 1024 1024))

;; Load config file
(org-babel-load-file (expand-file-name
		      "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (basic-c-compile bash-completion htmlize rust-mode slime slime-mode web-mode markdown-mode async exwm use-package))))
