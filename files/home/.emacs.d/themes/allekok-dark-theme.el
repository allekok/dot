;;; allekok-dark-theme.el --- Allekok dark theme

;; Copyright (C) 2019 allekok

;; Author: Payam <payambapiri.97@gmail.com>

;;; Code:
(require 'allekok-core-theme)

(deftheme allekok-dark)

(let* ((class '((class color) (min-colors 89)))
       ;; Font
       (font-family "NotoSansMono")
       (font-size-main 110)
       (font-size-mini 90)
       ;; Color
       (colors (get-colorname "dark"))
       (back-color (nth 0 colors))
       (fore-color (nth 1 colors))
       (keyword-color (nth 2 colors))
       (warn-color (nth 3 colors))

       (back-light-color "#333333")
       (fore-light-color "#AAAAAA")
       (comment-color back-light-color)
       (hl-color back-light-color)
       (region-color back-light-color)
       (builtin-color fore-color)
       (string-color fore-color)
       (var-color fore-color)
       (paren-match-color keyword-color))
  (custom-theme-set-faces
   'allekok-dark
   `(default ((,class (:family
		       ,font-family
		       :background ,back-color
		       :foreground ,fore-color
		       :height ,font-size-main
		       :weight medium))))
   `(mode-line ((,class (:foreground
			 ,fore-color
			 :background ,back-color
			 :height ,font-size-mini
			 :weight medium
			 :box (:color
			       ,back-color
			       :line-width 2)))))
   `(mode-line-inactive ((,class (:foreground
				  ,fore-light-color
				  :background ,back-color
				  :height ,font-size-mini
				  :weight medium
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
   `(font-lock-function-name-face ((,class (:weight bold))))
   `(font-lock-string-face ((,class (:foreground ,string-color))))
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
   `(eshell-prompt ((,class (:foreground ,keyword-color :weight medium))))
   `(dired-async-message ((,class (:foreground ,keyword-color))))
   `(web-mode-html-tag-face ((,class (:foreground ,fore-color))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,fore-color))))
   `(error ((,class (:foreground ,warn-color))))
   `(compilation-info ((,class (:foreground ,keyword-color))))
   `(org-todo ((,class (:foreground ,warn-color))))
   `(org-done ((,class (:foreground ,keyword-color))))
   `(font-lock-type-face ((,class (:foreground ,fore-color))))))

(provide-theme 'allekok-dark)

;;; allekok-dark-theme.el  ends here
