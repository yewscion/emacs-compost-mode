;;; compost.el --- A Note-taking Mode for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Christopher Rodriguez

;; Author: Christopher Rodriguez <yewscion@gmail.com>
;; Keywords: convenience, files, outlines

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

;;;; Customization

(require 'deadgrep)
(require 'pdf-annot)
(require 'ebib)

(defgroup compost nil
  "An implementation of a variation on the Zettelkasten method of
notetaking in GNU Emacs, leveraging org-mode, plain-text, and
pdf-tools to create a directory of notes."
  :group 'text)

(defcustom compost-main-directory (expand-file-name "~/.compost")
  "The directory for storing Your compost. Shouldn't end with a slash."
  :type 'directory
  :safe 'stringp
  :group 'compost)

(defcustom compost-meso-directory (expand-file-name "~/.compost/meso")
  "The directory for storing Your compost's meso notes. Shouldn't
end with a slash."
  :type 'directory
  :safe 'stringp
  :group 'compost)

(defcustom compost-thermo-directory (expand-file-name "~/.compost/thermo")
  "The directory for storing Your compost's thermo notes. Shouldn't
end with a slash."
  :type 'directory
  :safe 'stringp
  :group 'compost)

(defcustom compost-curing-directory (expand-file-name "~/.compost/curing")
  "The directory for storing Your compost's curing notes. Shouldn't
end with a slash."
  :type 'directory
  :safe 'stringp
  :group 'compost)

(defcustom compost-thermo-number-file (expand-file-name
                                (concat compost-thermo-directory
                                        "/.compost-number"))
  "The file storing how many thermo notes You have, which
increments on every number You assign." 
  :type 'file
  :safe 'stringp
  :group 'compost)

(defconst compost-id-char-set
  '(
    ?\x2654  ; 0
    ?\x2655  ; 1
    ?\x2656  ; 2
    ?\x2657  ; 3
    ?\x2658  ; 4
    ?\x2659  ; 5
    ?\x265A  ; 6
    ?\x265B  ; 7
    ?\x265C  ; 8
    ?\x265D  ; 9
    ?\x265E  ; 10
    ?\x265F  ; 11
    ?\x2660  ; 12
    ?\x2661  ; 13
    ?\x2662  ; 14
    ?\x2663  ; 15
    ?\x2664  ; 16
    ?\x2665  ; 17
    ?\x2666  ; 18
    ?\x2667  ; 19
    ?\x2680  ; 20
    ?\x2681  ; 21
    ?\x2682  ; 22
    ?\x2683  ; 23
    ?\x2684  ; 24
    ?\x2685  ; 25
    ?\x2686  ; 26
    ?\x2687  ; 27
    ?\x2688  ; 28
    ?\x2689  ; 29 *
    ?\x2704  ; 30
    ?\x2706  ; 31
    ?\x2707  ; 32
    ?\x2708  ; 33
    ?\x2709  ; 34
    ?\x270C  ; 35
    ?\x270D  ; 36
    ?\x270F  ; 37
    ?\x2711  ; 38
    ?\x2712  ; 39
    ?\x2713  ; 40
    ?\x2715  ; 41
    )
  "This is a set of characters used to encode the id number of compost thermo notes.")

;;;###autoload
(defun compost-search-meso (regex)
  "Search through the compost \"meso\" directory for matches of REGEX.

This is an ACTION.

Arguments
=========

REGEX <string>: A regular expression for which to search for matches.

Returns
=======

Return value of call to 'deadgrep'.

Impurities
==========

Used entirely for side effects: Calls 'deadgrep' using two
arguments, one obtained through a prompt."
  (interactive "sCompost Meso Search: ")
  (let ((deadgrep-display-buffer-function #'switch-to-buffer))
    (deadgrep regex compost-meso-directory)))

;;;###autoload
(defun compost-search-thermo (regex)
  "Search through the compost \"thermo\" directory for matches of REGEX.

This is an ACTION.

Arguments
=========

REGEX <string>: A regular expression for which to search for matches.

Returns
=======

Return value of call to 'deadgrep'.

Impurities
==========

Used entirely for side effects: Calls 'deadgrep' using two
arguments, one obtained through a prompt."
  (interactive "sCompost Thermo Search: ")
  (let ((deadgrep-display-buffer-function #'switch-to-buffer))
    (deadgrep regex compost-thermo-directory)))

;;;###autoload
(defun compost-search-curing (regex)
  "Search through the compost \"curing\" directory for matches of REGEX.

This is an ACTION.

Arguments
=========

REGEX <string>: A regular expression for which to search for matches.

Returns
=======

Return value of call to 'deadgrep'.

Impurities
==========

Used entirely for side effects: Calls 'deadgrep' using two
arguments, one obtained through a prompt."
  (interactive "sCompost Curing Search: ")
  (let ((deadgrep-display-buffer-function #'switch-to-buffer))
    (deadgrep regex compost-curing-directory)))

;;;###autoload
(defun compost-add-to-meso (&optional entry)
  "Adds a new entry into the configured 'compost-meso-directory.

This is an ACTION.

Arguments
=========

TIME <number> or <list> or <nil>: A valid time, as specified by
format-time-string. Usually either UNIX seconds or '(HI LO US
PS). nil will use the current time.

Returns
=======

Undefined.

Impurities
==========

Used entirely for Side Effects: Creates a new buffer associated
with a file in the compost directory for the current second in
local time, in which the user can add notes."  
  (interactive)
  (unless ebib--initialized
    (ebib-init))
  (let* ((entry
          (caar (ebib-read-entry "Compost Meso for Entry: "
                                 ebib--databases 'multiple)))
         (filename (concat compost-meso-directory "/" entry ".org")))
    (progn
      (find-file
       (file-truename
        filename))
      (if (not (file-exists-p filename))
          (insert (concat "* " entry "\n"
                          "[[ebib:" entry "][Ebib Entry]]\n")))
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key (kbd "C-c C-c") #'compost-meso-new-thought))))

;;;###autoload
(defun compost-add-to-thermo (&optional time)
  "Adds a new entry into the configured 'compost-thermo-directory.

This is an ACTION.

Arguments
=========

TIME <number> or <list> or <nil>: A valid time, as specified by
format-time-string. Usually either UNIX seconds or '(HI LO US
PS). nil will use the current time.

Returns
=======

Undefined.

Impurities
==========

Used entirely for Side Effects: Creates a new buffer associated
with a file in the compost directory for the current second in
local time, in which the user can add notes."  
  (interactive)
  (unless ebib--initialized
    (ebib-init))
  (find-file
   (file-truename
    (concat compost-thermo-directory "/" (compost-date time) ".txt"))))

;;;###autoload
(defun compost-add-to-curing (&optional topic time)
  "Adds a new entry into the configured 'compost-directory.

This is an ACTION.

Arguments
=========

TIME <number> or <list> or <nil>: A valid time, as specified by
format-time-string. Usually either UNIX seconds or '(HI LO US
PS). nil will use the current time.

Returns
=======

Undefined.

Impurities
==========

Used entirely for Side Effects: Creates a new buffer associated
with a file in the compost directory for the current second in
local time, in which the user can add notes."  
  (interactive "sTopic for Curing Compost: ")
  (unless ebib--initialized
    (ebib-init))
  (let ((filename (concat compost-curing-directory "/"
                          topic ".org")))
    (progn
      (find-file
       (file-truename
        filename))
      (if (not (file-exists-p filename))
          (insert (concat "* " topic "\n")))
      (insert (concat "** " (compost-date time) "\n")))))

;;;###autoload
(defun compost-transplant ()
  "Kill the current buffer with guardrails inserted before and
after, then yank it into the current 'other window'. Meant to be
used in a two-window setup, with the current buffer's contents
being added as a comment to the other buffer's contents, at the
current point of other buffer.

This is an ACTION.

Arguments
=========

None.

Returns
=======

Undefined.

Impurities
==========

Used entirely for Side Effects: Copies state of current buffer,
switches current buffer, alters text of new current buffer, and
switches again."
  (interactive)
  (progn
    (compost--kill-buffer)
    (other-window 1)
    (compost--yank-as-comment)
    (other-window 1)))

;;;###autoload
(defun compost-annotation-done (@click)
  (interactive "e")
  (let ((p1 (event-start @click)))
    (pdf-annot-put (pdf-annot-at-position p1) 'color "#006400")))

;;;###autoload
(defun compost-annotation-new (@click)
  (interactive "e")
  (let ((p1 (event-start @click)))
    (pdf-annot-add-text-annotation p1)))

;;;###autoload
(defvar compost-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "M" 'compost-add-to-meso)
    (define-key map "T" 'compost-add-to-thermo)
    (define-key map "C" 'compost-add-to-curing)
    (define-key map "m" 'compost-search-meso)
    (define-key map "t" 'compost-search-thermo)
    (define-key map "c" 'compost-search-curing)
    (define-key map "w" 'compost-transplant)
    (define-key map "l" 'compost-cure-thermo-link)
    (define-key map "L" 'compost-create-org-link)
    (define-key map "n" 'compost-add-thermo-number)
    (define-key map "<mouse-1>" 'compost-annotation-new)
    (define-key map "<mouse-3>" 'compost-annotation-done)
    map)
  "Keymap for Compost Prefix")

;;;###autoload
(fset 'compost-prefix compost-prefix-map)


(defun compost-date (&optional time)
  "Generates a datecode equivalent to the default for compost entries.

This is an ACTION.

Arguments
=========

TIME <number> or <list> or <nil>: A valid time, as specified by
format-time-string. Usually either UNIX seconds or '(HI LO US
PS). nil will use the current time.

Returns
=======

A <string> of the format \"YYYYMMDDTHHMMSSZ±ZONE\" for the
specified time, which is the default filename format for
org-journal files.

Impurities
==========

None if time is specified, otherwise relies on current system time."
  (let ((now (decode-time time)))
    (format
     "%d%02d%02dT%02d%02d%02dZ%05d"
     (nth 5 now)
     (nth 4 now)
     (nth 3 now)
     (nth 2 now)
     (nth 1 now)
     (nth 0 now)
     (/ (nth 8 now) 36))))

(defun compost--yank-as-comment ()
  "Yank the top of the kill ring into the current buffer as a comment.

This is an ACTION.

Arguments
=========

None.

Returns
=======

Undefined.

Impurities
==========

Used entirely for Side Effects: Modifies kill-ring and current buffer."
  (progn (yank)
         (comment-region (mark) (point))))

(defun compost--kill-buffer ()
  "Add the entirety of the current buffer to the kill ring, with guard rails.

This is an ACTION.

Arguments
=========

None.

Returns
=======

Undefined.

Impurities
==========

Used entirely for Side Effects: Modifies kill-ring and uses
current buffer state."
  (kill-new (concat
             "--------\n"
             (compost--remove-ending-newlines
              (compost--buffer-as-string))
             "\n"
             "--------\n"
             )))
(defun compost--kill-buffer-as-comment ()
  "Add the entire buffer to the kill ring as a comment of the current mode's
syntax.

This is an ACTION.

Arguments
=========

None.

Returns
=======

Undefined.

Impurities
==========

Used entirely for Side Effects: Modifies kill-ring and uses
current buffer state and mode."
  (kill-new (compost--comment-block-from-buffer)))

(defun compost--comment-block-from-buffer ()
  "Create a comment block out of the contents of the current buffer.

This is an ACTION.

Arguments
=========

None.

Returns
=======

Undefined.

Impurities
==========

Used entirely for Side Effects: Modifies kill-ring and uses
current buffer state and mode."
  (compost--create-comment-block (compost--buffer-as-string)))

(defun compost--create-comment-block (string)
  "Create a comment block using STRING as the contents.

This is an ACTION.

Arguments
=========

STRING <string>: The contents of the comment block.

Returns
=======

A <string> with the original STRING surrounded by guard rails and
with comment padding and characters prepended to each line.

Impurities
==========

Relies on current buffer's major mode for comment characters."
  (concat comment-start
          comment-padding
          "--------\n"
          (compost--comment-string string)
          "\n"
          comment-start
          comment-padding
          "--------\n"))

(defun compost--comment-string (string)
  "Comment STRING using the current major mode's comment syntax, after
removing any extraneous newlines.

This is an ACTION.

Arguments
=========

STRING <string>: The string to turn into a comment.

Returns
=======

A <string> representing the original STRING with comment padding
and characters prepended to each line.

Impurities
==========

Relies on current buffer's major mode for comment characters."
  (compost--comment-multiline-string
   (compost--remove-ending-newlines string)))

(defun compost--comment-multiline-string (string)
  "Comments STRING using the current mode's comment syntax, taking into
account that newlines will need a new comment character.

This is an ACTION.

Arguments
=========

STRING <string>: The string to turn into a comment.

Returns
=======

A <string> representing the original STRING with comment padding
and characters prepended to each line.

Impurities
==========

Relies on current buffer's major mode for comment characters."
  (concat comment-start
          comment-padding
          (string-replace "\n"
                          (concat "\n"
                                  comment-start
                                  comment-padding)
                          string)))

(defun compost--remove-ending-newlines (string)
  "Removes extra newlines at the end of STRING, leaving none.

This is a CALCULATION.

Arguments
=========

STRING <string>: The string to turn into a comment.

Returns
=======

A <string> representing the original STRING, but with no ending
newlines.

Impurities
==========

None."
  (replace-regexp-in-string "\n+\\'" "" string))

(defun compost--buffer-as-string ()
  "Get the entire contents of the current buffer as a string.

This is an ACTION.

Arguments
=========

None.

Returns
=======

A <string> representing the entire contents of the current
buffer, with no properties or extraneous information.

Impurities
==========

Relies on the current buffer state."
  (buffer-substring-no-properties (point-min) (point-max)))

;;;###autoload
(defun compost-create-org-link (filename)
  "Create an org link to a file in the compost thermo directory.

This is an ACTION.

Arguments
=========

FILENAME <string>: The file to be linked to.

Returns
=======

<undefined>


Impurities
==========
I/O, relies on state of underlying system."
  (interactive (list
                (read-file-name
                 "Which file should be linked? "
                 compost-thermo-directory)))
  (insert
   (concat "[[../thermo/"
           (file-name-nondirectory filename)
           "][(" (car (last (with-temp-buffer
                              (insert-file-contents filename)
                              (split-string (buffer-string) "\n" t))))
           ")]]")))

;;;###autoload
(defun compost-cure-thermo-link (filename)
  "Create an org link to a file in the compost thermo directory.

This is an ACTION.

Arguments
=========

FILENAME <string>: The file to be linked to.

Returns
=======

<undefined>


Impurities
==========
I/O, relies on state of underlying system."
  (interactive (list
                (read-file-name
                 "Which file should be linked? "
                 compost-thermo-directory)))
  (insert (compost--curing-link-to-thermo-section filename)))

(defun compost--curing-link-to-thermo-section (filename)
  (let* ((basename (file-name-nondirectory filename))
         (contents (f-read-text filename))
         (note-lines (split-string contents "\n" t))
         (reference-string
          (car (last note-lines)))
         (id-string
          (car (last note-lines 2))))
    (save-excursion
      (end-of-buffer)
      (insert
       (compost--generate-cured-thermo-section
        basename reference-string id-string contents))
      (substring-no-properties (org-store-link nil nil)))))

(defun compost--generate-cured-thermo-section
    (basename reference-string id-string file-contents)
  (concat
   "** [[../thermo/" basename "][<" id-string ">]]\n\n"
   ":PROPERTIES:\n:reference:      " reference-string "\n:END:\n\n"
          "#+begin_quote\n"
          file-contents
          "#+end_quote\n\n"))

(defun compost--number-to-base42-list (number)
  (let* ((highest-rank-needed (compost--highest-rank-needed number 42))
         (rank (compost-base-rank 42 highest-rank-needed)))
    (cond ((or (= number 0)
               (= rank 1))
           (list number))
          (t
           (cons (/ number rank)
                 (compost--number-to-base42-list (mod number rank)))))))

(defun compost--number-from-base42-list (number-list &optional total rank)
  (let* ((rank (if rank rank 0))
         (total (if total total 0))
         (factor (expt 42 rank)))
    (cond ((eq '() number-list)
           total)
          (t
           (compost--number-from-base42-list
            (reverse (seq-drop (reverse number-list) 1))
            (+ (* (car (last number-list)) factor) total)
            (1+ rank))))))

(defun compost--encode-number-list (number-list)
  (string-join
   (mapcar
    (lambda (x)
      (string (nth x compost-id-char-set)))
    number-list)))

(defun compost--decode-number-string (number-string)
  (mapcar
   (lambda (x)
     (cl-position x compost-id-char-set))
   (string-to-list number-string)))

(defun compost--highest-rank-needed (number base &optional guess)
  (let ((guess (if guess guess 0)))
    (if (> (expt base guess) number)
        guess
      (compost--highest-rank-needed number base (+ guess 1)))))

(defun compost-add-thermo-number ()
  (interactive)
    (if (not (file-exists-p compost-thermo-number-file))
        (with-temp-file compost-thermo-number-file
          (insert "0")))
    (let ((number (string-to-number (f-read-text
                                     compost-thermo-number-file))))
      (insert (compost-encode-id number))
      (with-temp-file compost-thermo-number-file
        (insert (number-to-string (1+ number))))))

(defun compost-encode-id (number)
  (interactive)
  (let ((our-list (compost--number-to-base42-list number)))
    (while (< (length our-list) 5)
      (push 0 our-list))
    (compost--encode-number-list our-list)))

(defun compost-decode-id (number)
  (interactive)
  (compost--number-from-base42-list (compost--decode-number-string number)))

(defun compost-base-rank (base rank)
  (if (<= rank 0)
      0
    (expt base (- rank 1))))

(fset 'compost-meso-new-thought
      (kmacro-lambda-form
       [?\M-> escape return ?\C-c ?\C-t ?t ?\"
              ?\" ?\S- ?p ?. ?\C-b ?\C-b ?\C-b ?\C-b tab]
       0 "%d"))


(provide 'compost)
;;; compost.el ends here
