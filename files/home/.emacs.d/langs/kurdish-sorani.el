;;; kurdish-sorani.el --- Quail package for inputting Kurdish-Sorani	-*- coding: utf-8;-*-

;; Author: Payam <payambapiri.97@gmail.com>
;; Keywords: mule, input method, Kurdish-Sorani

;;; Commentary:

;;; Code:

(require 'quail)

(quail-define-package
 "kurdish-sorani" "Kurdish-Sorani" "ئا" nil "Kurdish-Sorani input method.

Based on Kurdish-Sorani table in X Keyboard Configuration DB.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("`" ?`)
 ("~" ?~)
 ("1" ?١)
 ("2" ?٢)
 ("3" ?٣)
 ("4" ?٤)
 ("5" ?٥)
 ("6" ?٦)
 ("7" ?٧)
 ("8" ?٨)
 ("9" ?٩)
 ("0" ?٠)

 ("(" ?\))
 (")" ?\()

 ("Q" ?X)
 ("W" ?X)
 ("E" ?ه)
 ("R" ?ڕ)
 ("T" ?ط)
 ("Y" ?ێ)
 ("U" ?ء)
 ("I" ?ع)
 ("O" ?ؤ)
 ("P" ?ث)
 ("{" ?})
 ("}" ?{)

 ("A" ?آ)
 ("S" ?ش)
 ("D" ?ذ)
 ("F" ?إ)
 ("G" ?غ)
 ("H" ?‌)
 ("J" ?أ)
 ("K" ?ك)
 ("L" ?ڵ)

 ("Z" ?ض)
 ("X" ?ص)
 ("C" ?چ)
 ("V" ?ظ)
 ("B" ?ى)
 ("N" ?ة)
 ("M" ?ـ)
 ("<" ?>)
 (">" ?<)
 ("?" ?؟)

 ("q" ?ق)
 ("w" ?و)
 ("e" ?ە)
 ("r" ?ر)
 ("t" ?ت)
 ("y" ?ی)
 ("u" ?ئ)
 ("i" ?ح)
 ("o" ?ۆ)
 ("p" ?پ)
 ("[" ?\])
 ("]" ?\[)

 ("a" ?ا)
 ("s" ?س)
 ("d" ?د)
 ("f" ?ف)
 ("g" ?گ)
 ("h" ?ه)
 ("j" ?ژ)
 ("k" ?ک)
 ("l" ?ل)
 (";" ?؛)
 ("'" ?')

 ("z" ?ز)
 ("x" ?خ)
 ("c" ?ج)
 ("v" ?ڤ)
 ("b" ?ب)
 ("n" ?ن)
 ("m" ?م)
 ("," ?،)
 ("." ?.)
 ("/" ?/))

(provide 'kurdish-sorani)
;;; kurdish-sorani.el ends here
