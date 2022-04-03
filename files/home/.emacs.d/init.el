(let ((el-file (expand-file-name "config.el" user-emacs-directory))
      (org-file (expand-file-name "config.org" user-emacs-directory)))
  
  (or (and (file-exists-p el-file) (load el-file))
      (org-babel-load-file org-path)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(exwm web-mode elisp--witness--lisp))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
