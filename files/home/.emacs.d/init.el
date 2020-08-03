(setq gc-cons-threshold (* 100 1024 1024))

(if (file-exists-p (expand-file-name "config.el" user-emacs-directory))
    (load (expand-file-name "config" user-emacs-directory))
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory)))

(setq gc-cons-threshold (* 2 1024 1024))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(csharp-mode google-translate markdown-mode exwm xelb web-mode async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
