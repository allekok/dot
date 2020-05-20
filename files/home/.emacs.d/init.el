(setq gc-cons-threshold (* 150 1024 1024))

(load (expand-file-name "config" user-emacs-directory))

(custom-set-variables
 '(package-selected-packages
   '(markdown-mode exwm xelb web-mode async)))
