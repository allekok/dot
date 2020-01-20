(setq gc-cons-threshold (* 150 1024 1024))

;; Load config file
(let* ((conf (expand-file-name "config" user-emacs-directory))
       (conf-elc (concat conf ".elc"))
       (conf-org (concat conf ".org")))
  
  (if (file-exists-p (concat conf-elc))
      (load-file (concat conf-elc))
    (org-babel-load-file conf-org)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exwm xelb htmlize slime slime-mode web-mode markdown-mode async use-package))))
(put 'upcase-region 'disabled nil)
(put 'lowercase-region 'disabled nil)
