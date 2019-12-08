;;; kurdish-kurmanci.el --- Quail package for inputting Kurdish-Kurmanci	-*- coding: utf-8;-*-

;; Copyright (C) 2019 allekok.

;; Author: allekok <one@allekok.com>
;; Keywords: mule, input method, Kurdish-Kurmanci

;;; Commentary:

;;; Code:

(require 'quail)

(quail-define-package
 "kurdish-kurmanci" "Kurdish-Kurmanci" "Kû" nil "Kurdish-Kurmanci input method.

Based on Kurdish-Kurmanci table in X Keyboard Configuration DB.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("`" ?\")
 ("~" ?é)
 
 ("Q" ?Q)
 ("W" ?W)
 ("E" ?E)
 ("R" ?R)
 ("T" ?T)
 ("Y" ?Y)
 ("U" ?U)
 ("I" ?I)
 ("O" ?O)
 ("P" ?P)
 ("{" ?X)
 ("}" ?Û)

 ("A" ?A)
 ("S" ?S)
 ("D" ?D)
 ("F" ?F)
 ("G" ?G)
 ("H" ?H)
 ("J" ?J)
 ("K" ?K)
 ("L" ?L)

 ("Z" ?Z)
 ("X" ?X)
 ("C" ?C)
 ("V" ?V)
 ("B" ?B)
 ("N" ?N)
 ("M" ?M)
 ("<" ?Ê)
 (">" ?Ç)
 ("?" ?:)

 ("q" ?q)
 ("w" ?w)
 ("e" ?e)
 ("r" ?r)
 ("t" ?t)
 ("y" ?y)
 ("u" ?u)
 ("i" ?i)
 ("o" ?o)
 ("p" ?p)
 ("[" ?x)
 ("]" ?û)

 ("a" ?a)
 ("s" ?s)
 ("d" ?d)
 ("f" ?f)
 ("g" ?g)
 ("h" ?h)
 ("j" ?j)
 ("k" ?k)
 ("l" ?l)
 (";" ?ş)
 ("'" ?î)

 ("z" ?z)
 ("x" ?x)
 ("c" ?c)
 ("v" ?v)
 ("b" ?b)
 ("n" ?n)
 ("m" ?m)
 ("," ?ê)
 ("." ?ç)
 ("/" ?.))

;;; kurdish-kurmanci.el ends here
