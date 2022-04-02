;;; allekok-light-theme.el --- Allekok light theme

;; Author: Payam <payambapiri.97@gmail.com>

;;; Code:
(deftheme allekok-light)

(setq allekok-theme 'allekok-light)
(load (comp-lookup-eln
       (expand-file-name "allekok-core-theme.el"
			 (concat user-emacs-directory "themes"))))

;;; allekok-light-theme.el  ends here
