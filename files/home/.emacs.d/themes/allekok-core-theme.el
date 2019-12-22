(defun get-colorname (LD)
  (let ((colors
	 (split-string
	  (with-temp-buffer
	    (insert-file
	     (expand-file-name
	      (if (string= LD "dark")
		  ".color-dark" ".color-light")
	      (getenv "HOME")))
	    (buffer-string))
	  split-string-default-separators t)))
    colors))

(provide 'allekok-core-theme)
