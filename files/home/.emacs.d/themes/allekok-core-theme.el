(defun get-colorname (LD)
  (setenv "COLORNOW" LD)
  (let ((colors
	 (split-string
	  (with-temp-buffer
	    (insert-file-contents
	     (expand-file-name
	      (if (string= LD "dark")
		  ".color-dark" ".color-light")
	      (getenv "HOME")))
	    (buffer-string))
	  split-string-default-separators t)))
    colors))

(provide 'allekok-core-theme)
