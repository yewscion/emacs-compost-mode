;;; compost-drawing.el --- An input method for drawing Unicode Diagrams for Compost Notes  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Christopher Rodriguez

;; Author: Christopher Rodriguez <>
;; Keywords: convenience, extensions, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


(require 'quail)
(quail-define-package "compost-drawing" "Compost Drawing" "╳╳╳╳" nil
                      "Transliteration based input method for drawing simple diagrams using the
box drawing characters in unicode. No modifier keys (Shift, Caps, Alt,
etc) are needed to access the entire charset."
                      nil t t t t t t nil nil nil t)
(quail-define-rules
 ("q" ?┌)
 ("w" ?─)
 ("e" ?┐)
 ("r" ?┬)
 ("t" ?↔)
 ("y" ?↕)
 ("u" ?╎)
 ("i" ?╭)
 ("o" ?╮)
 ("p" ?🭯)
 ("[" ?🭭)
 ("]" ?╱)
 ("\\" ?╲)
 ("a" ?│)
 ("s" ?┼)
 ("d" ?│)
 ("f" ?├)
 ("g" ?┤)
 ("h" ?┊)
 ("j" ?╰)
 ("k" ?╯)
 ("l" ?🮧)
 (";" ?🮦)
 ("'" ?⟲)
 ("z" ?└)
 ("x" ?─)
 ("c" ?┘)
 ("v" ?┴)
 ("b" ?╌)
 ("n" ?┄)
 ("m" ?🮤)
 ("," ?🭮)
 ("." ?🭬)
 ("/" ?🮥)
 ("`" ?⃰)
 ("1" ?ᷧ)
 ("2" ?ᷩ)
 ("3" ?ͯ)
 ("4" ?⃤)
 ("5" ?⃣)
 ("6" ?⃟)
 ("7" ?⃞)
 ("8" ?⃢)
 ("9" ?⃝)
 ("0" ?⃠)
 ("-" ?🮮)
 ("=" ?╳)
 (" " ? ))
(provide 'compost-drawing)

;;; compost-drawing.el ends here
