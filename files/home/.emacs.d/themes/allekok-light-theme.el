;;; allekok-light-theme.el --- Allekok light theme

;; Author: Payam <payambapiri.97@gmail.com>

;;; Code:
(require 'allekok-core-theme)

(deftheme allekok-light)

(let* ((class '((class color) (min-colors 89)))
       ;; Font
       (font-family "NotoSansMono")
       (font-size-main 110)
       (font-size-mini 110)
       ;; Color
       (colors (get-colorname "light"))
       (back-color (nth 0 colors))
       (fore-color (nth 1 colors))
       (keyword-color (nth 2 colors))
       (warn-color (nth 3 colors)))
  (custom-theme-set-faces
   'allekok-light
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
				  ,fore-color
				  :background ,back-color
				  :height ,font-size-mini
				  :weight medium
				  :box (:color
					,back-color
					:line-width 2)))))
   `(cursor ((,class (:background ,keyword-color))))
   `(region ((,class (:background ,keyword-color :foreground ,back-color))))
   `(highlight ((,class (:background ,keyword-color :foreground ,back-color))))
   `(hl-line ((,class (:background ,back-color))))
   `(vertical-border ((,class (:foreground ,keyword-color))))
   `(show-paren-match ((,class (:foreground ,keyword-color
					    :weight normal))))
   `(font-lock-warning-face ((,class (:foreground ,warn-color))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword-color))))
   `(font-lock-builtin-face ((,class (:foreground ,fore-color))))
   `(font-lock-function-name-face ((,class (:weight normal))))
   `(font-lock-string-face ((,class :foreground ,fore-color)))
   `(font-lock-comment-face ((,class :background ,back-color)))
   `(font-lock-variable-name-face ((,class :foreground ,fore-color)))
   `(font-lock-constant-face ((,class :weight normal)))
   `(minibuffer-prompt ((,class :foreground ,keyword-color)))
   `(link ((,class (:foreground ,keyword-color :weight bold))))
   `(header-line ((,class (:background ,back-color))))
   `(eww-valid-certificate ((,class (:foreground ,keyword-color :weight normal))))
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
   `(font-lock-type-face ((,class (:foreground ,fore-color))))
   `(erc-notice-face ((,class (:foreground ,keyword-color))))))

(provide-theme 'allekok-light)

;;; allekok-light-theme.el  ends here
