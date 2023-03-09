;;; compost.el --- A Note-taking Mode for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Christopher Rodriguez

;; Author: Christopher Rodriguez <>
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

(defcustom compost-meso-directory (expand-file-name "~/.compost/meso")
  "The directory for storing Your compost. Shouldn't end with a slash."
  :type 'directory
  :safe 'stringp
  :group 'compost)

(defcustom compost-thermo-directory (expand-file-name "~/.compost/thermo")
  "The directory for storing Your compost. Shouldn't end with a slash."
  :type 'directory
  :safe 'stringp
  :group 'compost)

(defcustom compost-curing-directory (expand-file-name "~/.compost/curing")
  "The directory for storing Your compost. Shouldn't end with a slash."
  :type 'directory
  :safe 'stringp
  :group 'compost)

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
                          "[[ebib:" entry "][Ebib Entry]]\n"))))))

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
    (define-key map "m" 'compost-add-to-meso)
    (define-key map "t" 'compost-add-to-thermo)
    (define-key map "c" 'compost-add-to-curing)
    (define-key map "M" 'compost-search-meso)
    (define-key map "T" 'compost-search-thermo)
    (define-key map "C" 'compost-search-curing)
    (define-key map "w" 'compost-transplant)
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

(defun compost-create-link (filename)
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

(provide 'compost)
;;; compost.el ends here
