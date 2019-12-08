(setq gc-cons-threshold (* 150 1024 1024))

;; Load config file
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c54c58274300ec35b5f1e0580be74c3983d04d139a4626c777204e741c4a1f8" "2e3e2cba9ec95beb78bdbcf12b2045cc29ef61d918c3441f0421ab299b26149f" allekok-dark allekok-light default)))
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
