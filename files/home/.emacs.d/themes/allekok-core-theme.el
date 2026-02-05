(defun get-colorname (LD)
  (let ((color (if (eq LD 'allekok-dark)
		   "dark"
		 "light")))
    (setenv "COLORNOW" color)
    (let ((colors
	   (split-string
	    (with-temp-buffer
	      (insert-file-contents
	       (expand-file-name
		(format ".color-%s" color)
		(getenv "HOME")))
	      (buffer-string))
	    split-string-default-separators t)))
      colors)))

(let* ((class '((class color) (min-colors 89)))
       ;; Font
       (font-family "SourceCodePro")
       (font-size-main 110)
       (font-size-mini 100)
       ;; Color
       (colors (get-colorname allekok-theme))
       (back-color (nth 0 colors))
       (fore-color (nth 1 colors))
       (keyword-color (nth 2 colors))
       (warn-color (nth 3 colors)))
  (custom-theme-set-faces
   allekok-theme
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
			 :box nil))))
   `(mode-line-inactive ((,class (:foreground
				  ,fore-color
				  :background ,back-color
				  :height ,font-size-mini
				  :box nil))))
   `(cursor ((,class (:background ,keyword-color))))
   `(region ((,class (:background ,keyword-color :foreground ,back-color))))
   `(highlight ((,class (:background ,keyword-color :foreground ,back-color))))
   `(hl-line ((,class (:background ,back-color))))
   `(vertical-border ((,class (:foreground ,fore-color))))
   `(show-paren-match ((,class (:foreground ,keyword-color :weight bold))))
   `(font-lock-warning-face ((,class (:foreground ,warn-color))))
   `(font-lock-keyword-face ((,class (:foreground ,fore-color))))
   `(font-lock-builtin-face ((,class (:foreground ,fore-color))))
   `(font-lock-function-name-face ((,class (:foreground ,fore-color))))
   `(font-lock-string-face ((,class :foreground ,fore-color)))
   `(font-lock-comment-face ((,class :background ,back-color)))
   `(font-lock-variable-name-face ((,class :foreground ,fore-color)))
   `(font-lock-constant-face ((,class :foreground ,fore-color :weight medium)))
   `(minibuffer-prompt ((,class :foreground ,fore-color)))
   `(link ((,class (:foreground ,keyword-color :weight medium))))
   `(header-line ((,class (:background ,back-color))))
   `(eww-valid-certificate ((,class (:foreground ,keyword-color :weight medium))))
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
   `(org-document-title ((,class (:foreground ,keyword-color))))
   `(org-document-info ((,class (:foreground ,keyword-color))))
   `(erc-notice-face ((,class (:foreground ,keyword-color))))))

(provide-theme allekok-theme)
