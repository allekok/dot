;;; allekok-dark-theme.el --- Allekok dark theme

;; Copyright (C) 2019 allekok

;; Author: Payam <one@allekok.com>

;;; Code:

(deftheme allekok-dark)

(let* ((class '((class color) (min-colors 89)))
       ;; Font
       (font-family "Liberation Mono")
       (font-size-main 130)
       (font-size-mini 110)
       ;; Color
       (back-color "#222")
       (back-light-color "#333")
       (fore-color "#ffffff")
       (fore-light-color "#aaa")
       (keyword-color "#0fa")
       (warn-color keyword-color)
       (builtin-color fore-color)
       (string-color fore-color)
       (comment-color back-light-color)
       (var-color fore-color)
       (hl-color back-light-color)
       (region-color "#444")
       (paren-match-color warn-color))
  (custom-theme-set-faces
   'allekok-dark
   `(default ((,class (:family
		       ,font-family
		       :background ,back-color
		       :foreground ,fore-color
		       :height ,font-size-main
		       :weight normal))))
   `(mode-line ((,class (:foreground
			 ,fore-color
			 :background ,back-color
			 :height ,font-size-mini
			 :weight bold
			 :box (:color
			       ,back-color
			       :line-width 2)))))
   `(mode-line-inactive ((,class (:foreground
				  ,fore-light-color
				  :background ,back-color
				  :height ,font-size-mini
				  :weight bold
				  :box (:color
					,back-color
					:line-width 2)))))
   `(cursor ((,class (:background ,keyword-color))))
   `(region ((,class (:background ,region-color))))
   `(hl-line ((,class (:background ,hl-color))))
   `(vertical-border ((,class (:foreground ,fore-light-color))))
   `(show-paren-match ((,class (:foreground ,paren-match-color
					    :weight bold))))
   `(font-lock-warning-face ((,class (:foreground ,warn-color))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword-color))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin-color))))
   `(font-lock-function-name-face ((,class (:italic t))))
   `(font-lock-string-face ((,class (:foreground ,string-color
						 :italic t))))
   `(font-lock-comment-face ((,class (:background ,comment-color))))
   `(font-lock-variable-name-face ((,class (:foreground ,var-color))))
   `(font-lock-constant-face ((,class (:weight bold))))
   `(minibuffer-prompt ((,class (:foreground ,keyword-color))))
   `(link ((,class (:foreground ,keyword-color :weight bold))))
   `(header-line ((,class (:background ,back-light-color))))
   `(eww-valid-certificate ((,class (:foreground ,keyword-color :weight bold))))
   `(isearch ((,class (:underline t))))
   `(isearch-fail ((,class (:strike-through t))))
   `(lazy-highlight ((,class (:underline t :foreground ,keyword-color))))
   `(eshell-prompt ((,class (:foreground ,keyword-color :weight bold))))
   `(font-lock-type-face ((,class (:foreground ,fore-color))))))

(provide-theme 'allekok-dark)

;;; allekok-dark-theme.el  ends here
