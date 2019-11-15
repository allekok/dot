(setq gc-cons-threshold (* 150 1024 1024))

;; Load config file
(let ((config-file
       (expand-file-name "config.org" user-emacs-directory))
      (compiled-config-file
       (expand-file-name "config.elc" user-emacs-directory)))
  (if (file-exists-p compiled-config-file)
      (load-file compiled-config-file)
    (org-babel-load-file config-file)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (htmlize rust-mode slime slime-mode web-mode markdown-mode async exwm use-package))))
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
