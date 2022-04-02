;;; allekok-dark-theme.el --- Allekok dark theme

;; Author: Payam <payambapiri.97@gmail.com>

;;; Code:
(deftheme allekok-dark)

(setq allekok-theme 'allekok-dark)
(load (comp-lookup-eln
       (expand-file-name "allekok-core-theme.el"
			 (concat user-emacs-directory "themes"))))

;;; allekok-dark-theme.el  ends here
