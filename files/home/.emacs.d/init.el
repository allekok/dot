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
 '(package-selected-packages '(haskell-mode exwm xelb web-mode markdown-mode async))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
