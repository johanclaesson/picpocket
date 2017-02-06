;;; picpocket.el --- Image viewer -*- lexical-binding: t; coding: utf-8-unix -*-

;; Copyright (C) 2017 Johan Claesson
;; Author: Johan Claesson <johanclaesson@bredband.net>
;; Maintainer: Johan Claesson <johanclaesson@bredband.net>
;; Version: 28
;; Keywords: multimedia
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Picpocket is an image/picture viewer which requires GNU Emacs 24+
;; compiled with ImageMagick.  It has commands for:
;;
;; * File operations on the picture files (delete, move, copy, hardlink).
;; * Scale and rotate the picture.
;; * Associate pictures with tags which are saved to disk.
;; * Filter pictures according to tags.
;; * Customizing keystrokes for quick tagging and file operations.
;; * Undo and browse history of undoable commands.
;;
;;
;; Main entry point
;; ----------------
;;
;; Command: picpocket
;;
;; View the pictures in the current directory.
;;
;;
;; Main keybindings
;; ----------------
;;
;; Space     - Next picture
;; BackSpace - Previous picture
;; r         - Rename picture file
;; c         - Copy picture file
;; k         - Delete picture file
;; t         - Edit tags
;; s         - Slide-show mode
;; [         - Rotate counter-clockwise
;; ]         - Rotate clockwise
;; +         - Scale in
;; -         - Scale out
;; u         - Undo
;; M-u       - View history of undoable actions
;; e         - Customize keystrokes (see below)
;; TAB f     - Toggle full-screen
;; TAB r     - Toggle recursive inclusion of pictures in sub-directories
;;
;; With prefix argument many of the commands will operatate on all the
;; pictures in the current list instead of just the current picture.
;;
;;
;; Disclaimer
;; ----------
;;
;; Picpocket will secretly do stuff in the background with idle
;; timers.  This includes to load upcoming pictures into the image
;; cache.  The intention is that they should block Emacs for so short
;; periods that it is not noticable.  But if you want to get rid of
;; them set `picp-inhibit-timers' or kill the picpocket buffer.
;;
;; Picpocket is to be considered beta software.  Keybindings,
;; variables and such may change in future versions.  Tag database
;; file format will remain backwards compatible though.
;;
;;
;; Keystroke customization
;; -----------------------
;;
;; Keystokes can be bound to commands that move/copy pictures into
;; directories and/or tag them.  The following creates commands on
;; keys 1 though 5 to tag according to liking.  Also creates some
;; commands to move picture files to directories according to genre.
;; Finally creates also one command to copy pictures to a backup
;; directory in the user's home directory.
;;
;; (defvar my-picp-alist
;;   '((?1 tag "bad")
;;     (?2 tag "sigh")
;;     (?3 tag "good")
;;     (?4 tag "great")
;;     (?5 tag "awesome")
;;     (?F move "fantasy")
;;     (?S move "scifi")
;;     (?P move "steampunk")
;;     (?H move "horror")
;;     (?U move "urban-fantasy")
;;     (?B copy "~/backup")))
;;
;; (setq picp-keystroke-alist 'my-picp-alist)
;;
;; Digits and capital letters with no modifiers is reserved for these
;; kind of user keybindings.
;;
;; It is recommended to set `picp-keystroke-alist' to a symbol as
;; above.  That makes the command `picp-edit-keystrokes' (bound to `e'
;; in picpocket buffer) jump to your definition for quick changes.
;; Edit the list and type M-C-x to save it.
;;
;; See the doc of `picp-keystroke-alist' for about the same thing but
;; with a few more details.

;;
;; Tag database
;; ------------
;;
;; Tags associated with pictures are saved to disk.  By default to
;; ~/.emacs.d/picpocket/.  This database maps the sha1 checksum of
;; picture files to list of tags.  This implies that you can rename or
;; move around the file anywhere and the tags will still be remembered
;; when you view it with picpocket again.
;;
;; If you change the picture file content the sha1 checksum will
;; change.  For example this would happen if you rotate or crop the
;; picture with an external program.  That will break the association
;; between sha1 checksum and tags.  However picpocket also stores the
;; file name for each entry of tags.  The command
;; `picpocket-db-update' will go through the database and offer to
;; recover such lost associations.
;;
;; If you change the file-name and the file content at the same time
;; there is no way to recover automatically.

;;; Code:

(eval-when-compile
  (require 'time-date)
  (require 'ido))

(require 'cl-lib)
(require 'dired)
(require 'subr-x)
(require 'ring)
(require 'ewoc)

;;; Options

(defgroup picpocket nil "Picture viewer."
  :group 'picpocket)

(defcustom picp-keystroke-alist nil
  "Symbol of variable with an alist of picpocket keystrokes.
\\<picp-mode-map>
Elements in the alist have the form (KEY ACTION TARGET).

KEY is a single character, a string accepted by `kbd' or a vector
accepted by `define-key'.

ACTION is tag, move, copy or hardlink.

TARGET is a string.  For ACTION tag it is the tag to add.  For
the other actions it is the destination directory.

Example:

 (defvar my-picp-alist
   '((?1 tag \"bad\")
     (?2 tag \"sigh\")
     (?3 tag \"good\")
     (?4 tag \"great\")
     (?5 tag \"awesome\")))

 (setq picp-keystroke-alist 'my-picp-alist)

There exist a convenience command `picp-edit-keystrokes' that
will jump to the definition that the variable
`picp-keystroke-alist' points to.  It is bound to
\\[picp-keystroke-alist] in the picpocket buffer."
  :risky t
  :type 'symbol)

(defcustom picp-filter-consider-dir-as-tag t
  "Whether filter will consider the directory as a tag.

If non-nil add the containing directory name to the list of tags
stored in database.  This matters only when considering a filter.
For example the potential extra tag is not shown in the list of
tags in the header line.

The special value `all' means add all directories in the whole
path (after all soft links have been resolved)."
  :type 'boolean)

(defvar picp-filter-ignore-hyphens t
  "If non-nil the filter ignores underscores and dashes.")


(defcustom picp-fit :x-and-y
  "Fit picture size to window when non-nil."
  :type '(choice (const :tag "Fit to both width and height" :x-and-y)
                 (const :tag "Fit to width" :x)
                 (const :tag "Fit to height" :y)
                 (const :tag "Show picture in it's natural size" nil)))

(defcustom picp-scale 100
  "Picture scaling in percent."
  :type 'integer)

(defcustom picp-header t
  "If non-nil display a header line in picpocket buffer."
  :type 'boolean)

(defcustom picp-header-line-format '(:eval (picp-header-line))
  "The value for `header-line-format' in picpocket buffers.
Enabled if `picp-header' is non-nil."
  :type 'sexp)

(defcustom picp-header-full-path nil
  "If non-nil display the whole file path in header line."
  :type 'boolean)

(defcustom picp-tags-style :list
  "How a list of tags are displayed."
  :type '(choice (const :tag "Org style :tag1:tag2:" :org)
                 (const :tag "Lisp list style (tag1 tag2)" :list)))

(defcustom picp-confirm-delete (not noninteractive)
  "If non-nil let user confirm file delete."
  :type 'boolean)

(defcustom picp-backdrop-command nil
  "Command to set desktop backdrop with.
\\<picp-mode-map>
Used by `picp-set-backdrop' bound to \\[picp-backdrop-command] in
picpocket buffer.  If this variable is nil then
`picp-set-backdrop' will attempt to find an usable command and
assign it to this variable."
  :type 'string)

(defcustom picp-recursive nil
  "If non-nil include sub-directories recursively.
\\<picp-mode-map>
This variable can be toggled with the `picp-toggle-recursive'
command.  It is bound to \\[picp-toggle-recursive] in the
picpocket buffer."
  :type 'boolean)

(defcustom picp-follow-symlinks t
  "If non-nil follow symlinks while traversing directories recursively.

Symlinks that points to picture files are always followed.
This option concerns only symlinks that points to directories."
  :type 'boolean)

(defcustom picp-destination-dir "~/"
  "Destination dir if `picp-destination-relative-current' is nil.

Destination dir is the default target dir for move, copy and
hardlink commands."
  :type 'directory)

(defcustom picp-destination-relative-current t
  "If non-nil the destination dir is the current directory.
\\<picp-mode-map>
If nil it is the value of variable `picp-destination-dir'.  This
variable can be toggled with the command
`picp-toggle-destination-dir' bound to
\\[picp-toggle-destination-dir] in the picpocket buffer."
  :type 'boolean)

(defcustom picp-undo-thumbnails-size 7
  "The heigth of picpocket undo thumbnails.
Specified in number of default line heigths."
  :type 'integer)

;;; Internal variables

(defconst picp-version 28)
(defconst picp-buffer "*picpocket*")
(defconst picp-undo-buffer "*picpocket-undo*")

(defvar picp-max-undo-thumbnails 4)
(defvar picp-gimp-executable (executable-find "gimp"))
(defvar picp-look-ahead-max 5)
(defvar picp-frame nil)
(defvar picp-old-frame nil)
(defvar picp-last-action nil)
(defvar picp-last-arg nil)
(defvar picp-debug nil)
(defvar picp-sum 0)
(defvar picp-picture-regexp nil)
(defvar picp-last-look-ahead nil)
(defvar picp-clock-alist nil)
(defvar picp-adapt-to-window-size-change t)
(defvar picp-entry-function nil)
(defvar picp-entry-args nil)
(defvar picp-filter nil)
(defvar picp-filter-index nil)
(defvar picp-compute-filter-index-from-scratch nil)
(defvar picp-filter-match-count-done nil)
(defvar picp-filter-match-count nil)
(defvar picp-window-size nil
  "The current window size in pixels.
This is kept for the benefit of the idle timer functions that do
not necessarily run with the picpocket window selected.")
(defvar picp-header-text "")
(defvar picp-demote-warnings nil)
(defvar picp-sha1sum-executable nil)
(defvar picp-debug-idle-timers nil)
(defvar picp-old-keystroke-alist nil)
(defvar picp-default-backdrop-commands
  '(("display" . "-window root")
    ("feh" . "--bg-file")
    ("hsetroot" . "-tile")
    ("xsetbg")))
(defvar picp-tag-completion-table (make-vector 127 0))
(defvar picp-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map [tab] 'completion-at-point)
    map)
  "Keymap used for completing tags in minibuffer.")

(defvar picp-undo-list-size 25)
(defvar picp-undo-ring nil)


;; Variables displayed in the header-line must be marked as risky.
(dolist (symbol '(picp-index
                  picp-list-length
                  picp-current
                  picp-filter
                  picp-filter-index
                  picp-filter-match-count
                  picp-header-text))
  (put symbol 'risky-local-variable t))


;;; The list of pictures

(cl-defstruct picp-pos
  current
  index
  filter-index)

(defvar picp-list nil
  "The `picp-list' is a double-linked list of all pictures in directory.
The car contains a `picp-pic' struct whose prev slot points to
the previous cons cell.  The next cell is in the cdr.

Note that this is a circular data structure and `print-circle'
should be non-nil when printing it.")
(defvar picp-list-length nil)
(defvar picp-current nil)
(defvar picp-index 0
  "The `picp-index' is starting from 1 (incompatible with elt).
When there are no pictures in the list `picp-index' is zero.")

(cl-defstruct picp-pic
  prev
  dir
  file
  sha
  size
  bytes
  (rotation 0.0))


(defsubst picp-pic (pic)
  (car (or pic picp-current)))


(defsubst picp-prev (&optional pic)
  (picp-pic-prev (picp-pic pic)))

(defsubst picp-dir (&optional pic)
  (picp-pic-dir (picp-pic pic)))

(defsubst picp-file (&optional pic)
  (picp-pic-file (picp-pic pic)))

(defsubst picp-sha (&optional pic)
  (picp-pic-sha (picp-pic pic)))

(defsubst picp-size (&optional pic)
  (picp-pic-size (picp-pic pic)))

(defsubst picp-bytes (&optional pic)
  (picp-pic-bytes (picp-pic pic)))

(defsubst picp-rotation (&optional pic)
  (picp-pic-rotation (picp-pic pic)))


(defsubst picp-set-prev (pic value)
  (setf (picp-pic-prev (picp-pic pic)) value))

(defsubst picp-set-dir (pic value)
  (setf (picp-pic-dir (picp-pic pic)) value))

(defsubst picp-set-file (pic value)
  (setf (picp-pic-file (picp-pic pic)) value))

(defsubst picp-set-sha (pic value)
  (setf (picp-pic-sha (picp-pic pic)) value))

(defsubst picp-set-size (pic value)
  (setf (picp-pic-size (picp-pic pic)) value))

(defsubst picp-set-bytes (pic value)
  (setf (picp-pic-bytes (picp-pic pic)) value))

(defsubst picp-set-rotation (pic value)
  (setf (picp-pic-rotation (picp-pic pic)) value))


;;; Macros


(defmacro picp-command (&rest body)
  "Macro used in normal picpocket commands.
It is used in all commands except for those that switch to
another buffer (those use `picp-bye-command').  It takes care of
common stuff that shall be done before and after all commands in
picpocket mode.  The convention is to invoke this macro in the
body of all command functions.  It is not used in subroutines
because that would make it harder to verify that it is used in
all commands (and preferably only used one time).
\(fn &rest BODY)"
  (declare (indent defun)
           (debug (body)))
  `(progn
     (picp-ensure-picpocket-buffer)
     (let ((rc (progn ,@body)))
       (picp-ensure-picpocket-buffer)
       (picp-update-buffer)
       (when (buffer-live-p (get-buffer picp-undo-buffer))
         (picp-update-undo-buffer))
       rc)))

(defmacro picp-bye-command (&rest body)
  "Macro used in picpocket commands that switch to another buffer.
See `picp-command'.
\(fn &rest BODY)"
  (declare (indent defun)
           (debug (body)))
  `(progn
     (picp-ensure-picpocket-buffer)
     ,@body))



(cl-defmacro picp-when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY.
\(fn (VAR VALUE) &rest BODY)"
  (declare (debug ((symbolp form) body))
           (indent 1))
  `(let ((,var ,value))
     (when ,var ,@body)))

(defmacro picp-time-string (&rest forms)
  (declare (indent defun)
           (debug (body)))
  `(picp-sec-string (cadr (picp-time ,@forms))))

(defun picp-sec-string (time)
  (let ((sec (format "%.06f" (float-time time))))
    (concat (substring sec 0 -3)
            "_"
            (substring sec -3)
            "s")))

(defmacro picp-time (&rest forms)
  "Evaluate FORMS and return (rc time).
The reason it does not return (rc . time) is to be able to bind
with `cl-multiple-value-bind' etc."
  (declare (indent defun)
           (debug (body)))
  (let ((before (make-symbol "before"))
        (rc (make-symbol "rc")))
    `(let ((,before (current-time))
           (,rc (progn ,@forms)))
       (list ,rc (time-since ,before)))))

(defmacro picp-clock (&rest body)
  (declare (indent defun))
  (let ((thing (caar body)))
    `(picp-clock-thing ',thing ,@body)))

(defmacro picp-clock-thing (thing &rest body)
  (declare (indent 1))
  (let ((rc (make-symbol "rc"))
        (s (make-symbol "s"))
        (sum (make-symbol "sum")))
    `(cl-destructuring-bind (,rc ,s) (picp-time (progn ,@body))
       (let ((,sum (assq ,thing picp-clock-alist)))
         (if ,sum
             (setcdr ,sum (time-add ,s (cdr ,sum)))
           (push (cons ,thing ,s) picp-clock-alist)))
       ,rc)))

(defmacro picp-with-clock (title &rest body)
  (declare (debug (stringp body))
           (indent 1))
  `(let (picp-clock-alist)
     (prog1
         (progn ,@body)
       (picp-clock-report ,title))))


;;; Picp mode

(define-derived-mode picp-base-mode special-mode "picpocket-base"
  "Base major mode for buffers with images.
This mode is not used directly.  Other modes inherit from this mode."
  (buffer-disable-undo)
  ;; Ensure imagemagick is preferred.
  (unless (eq 'imagemagick (cdar image-type-file-name-regexps))
    (let ((entry (rassq 'imagemagick image-type-file-name-regexps)))
      (setq-local image-type-file-name-regexps
                  (cons entry
                        (delete entry
                                (cl-copy-list image-type-file-name-regexps))))))
  (setq-local image-type-header-regexps nil)
  (when (boundp 'image-scaling-factor)
    (setq-local image-scaling-factor 1.0))
  ;; image-map is embedded as text property on all images,
  ;; we do not want that in this buffer.
  (when (boundp 'image-map)
    (setq-local image-map nil))
  (setq truncate-lines t
        auto-hscroll-mode nil))

(define-derived-mode picp-mode picp-base-mode "picpocket"
  "Major mode for the main *picpocket* buffer."
  (picp-db-init)
  (picp-db-compile-tags-for-completion)
  (setq header-line-format (when picp-header
                             picp-header-line-format)
        vertical-scroll-bar nil
        cursor-type nil
        left-fringe-width 0
        right-fringe-width 0)
  ;; Call set-window-buffer to update the fringes.
  (set-window-buffer (selected-window) (current-buffer))
  (when (eq (selected-frame) picp-frame)
    (setq mode-line-format nil))
  (add-hook 'kill-emacs-hook #'picp-delete-trashcan)
  (add-hook 'kill-buffer-hook #'picp-save-journal nil t)
  (add-hook 'kill-buffer-hook #'picp-cleanup-hooks nil t)
  (add-hook 'window-size-change-functions #'picp-window-size-change-function)
  (add-hook 'buffer-list-update-hook #'picp-maybe-update-keymap)
  (add-hook 'buffer-list-update-hook #'picp-maybe-rescale)
  (picp-init-timers)
  (picp-update-keymap))

(defun picpocket-unload-function ()
  (message "Unloading picpocket")
  (picp-cancel-timers)
  (picp-delete-trashcan)
  (remove-hook 'kill-emacs-hook #'picp-delete-trashcan)
  (remove-hook 'window-size-change-functions #'picp-window-size-change-function)
  (remove-hook 'buffer-list-update-hook #'picp-maybe-update-keymap)
  (remove-hook 'buffer-list-update-hook #'picp-maybe-rescale)
  (remove-hook 'focus-in-hook #'picp-focus)
  (remove-hook 'minibuffer-setup-hook #'picp-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'picp-minibuffer-exit)
  nil)

(let ((map (make-sparse-keymap))
      (toggle-map (make-sparse-keymap
                   (concat "Toggle: [f - fullscreen]"
                           " [h - header]"
                           " [d - destination]"
                           " [r - recursive]"
                           " [l - follow symlinks]"
                           " [D - debug]"))))
  (define-key toggle-map [?f] #'picp-toggle-fullscreen-frame)
  (define-key toggle-map [?d] #'picp-toggle-destination-dir)
  (define-key toggle-map [?h] #'picp-toggle-header)
  (define-key toggle-map [?r] #'picp-toggle-recursive)
  (define-key toggle-map [?l] #'picp-toggle-follow-symlinks)
  (define-key toggle-map [?D] #'picp-toggle-debug)

  (suppress-keymap map)
  (define-key map [tab] toggle-map)
  (define-key map [backspace] #'picp-previous)
  (define-key map [prior] #'picp-previous)
  (define-key map [?p] #'picp-previous)
  (define-key map [?\s] #'picp-next)
  (define-key map [next] #'picp-next)
  (define-key map [?n] #'picp-next)
  (define-key map [?d] #'picp-dired)
  (define-key map [?v] #'picp-visit-file)
  (define-key map [?e] #'picp-edit-keystrokes)
  (define-key map [home] #'picp-home)
  (define-key map [?k] #'picp-delete-file)
  (define-key map [(control ?d)] #'picp-delete-file)
  (define-key map [deletechar] #'picp-delete-file)
  (define-key map [end] #'picp-end)
  (define-key map [?q] #'picp-quit)
  (define-key map [?s] #'picp-slideshow)
  (define-key map [?r] #'picp-rename)
  (define-key map [?m] #'picp-move)
  (define-key map [?c] #'picp-copy)
  (define-key map [?l] #'picp-hardlink)
  (define-key map [(meta ?m)] #'picp-move-by-keystroke)
  (define-key map [(meta ?c)] #'picp-copy-by-keystroke)
  (define-key map [(meta ?l)] #'picp-hardlink-by-keystroke)
  (define-key map [(meta ?t)] #'picp-tag-by-keystroke)
  (define-key map [?t] #'picp-edit-tags)
  (define-key map [?z] #'picp-repeat)
  (define-key map [?g] #'picp-revert)
  (define-key map [(meta ?b)] #'picp-set-backdrop)
  (define-key map [?.] #'picp-dired-up-directory)
  (define-key map [?f] #'picp-set-filter)
  (define-key map [(meta ?f)] #'picp-set-filter-by-keystroke)
  (define-key map [?\[] #'picp-rotate-counter-clockwise)
  (define-key map [?\]] #'picp-rotate-clockwise)
  (define-key map [?{] #'picp-rotate-counter-clockwise-10-degrees)
  (define-key map [?}] #'picp-rotate-clockwise-10-degrees)
  (define-key map [(meta ?\[)] #'picp-reset-rotation)
  (define-key map [(meta ?\])] #'picp-reset-rotation)
  (define-key map [?+] #'picp-scale-in)
  (define-key map [?=] #'picp-scale-in)
  (define-key map [?-] #'picp-scale-out)
  (define-key map [?0] #'picp-reset-scale)
  (define-key map [(meta ?a)] #'picp-fit-to-width-and-height)
  (define-key map [(meta ?w)] #'picp-fit-to-width)
  (define-key map [(meta ?h)] #'picp-fit-to-height)
  (define-key map [(meta ?n)] #'picp-no-fit)
  (define-key map [?i ?a] #'picp-fit-to-width-and-height)
  (define-key map [?i ?w] #'picp-fit-to-width)
  (define-key map [?i ?h] #'picp-fit-to-height)
  (define-key map [?i ?n] #'picp-no-fit)
  (define-key map [??] #'picp-help)
  (define-key map [left] #'scroll-right)
  (define-key map [right] #'scroll-left)
  (define-key map [(control ?b)] #'scroll-right)
  (define-key map [(control ?f)] #'scroll-left)
  (define-key map [?u] #'picp-undo)
  (define-key map [(meta ?u)] #'picp-visit-undo-list)
  (define-key map [?j] #'picp-jump)
  (define-key map [?o] #'picp-gimp-open)
  (define-key map [??] #'picp-help)
  (setq picp-mode-map map))


;;; Entry points

;;;###autoload
(defun picpocket ()
  "View the pictures in the current directory."
  (interactive)
  (let ((selected-file (cond ((buffer-file-name)
                              (file-truename (buffer-file-name)))
                             ((eq major-mode 'dired-mode)
                              (dired-get-filename nil t))
                             ((and (eq major-mode 'picp-mode)
                                   picp-current)
                              (picp-absfile))
                             (t nil))))
    (picpocket-dir default-directory selected-file)))

(defun picpocket-dir (dir &optional selected-file)
  "View the pictures in DIR starting with SELECTED-FILE."
  (setq picp-entry-function 'picpocket-dir
        picp-entry-args (list dir))
  (let ((files (picp-file-list dir)))
    (picp-create-buffer files selected-file dir)))

(defun picpocket-files (files &optional selected-file)
  "View the list of image files in FILES starting with SELECTED-FILE."
  (setq picp-entry-function 'picpocket-files
        picp-entry-args (list files))
  (picp-create-buffer files selected-file))

;;;###autoload
(defun picpocket-db-update ()
  "Manage the tag database.

Enter a special buffer where any suspicious database entries are
listed.  Suspicious entries are for example when files that have
disappeared.  Maybe they have been deleted outside of picpocket.
And the entries in picpocket now points to nowhere.  If there are
any such entries they will be listed in this buffer.  And there
will be an offer to clean up those entries from the database.

Note that this command can take some time to finish since it goes
through the entire database."
  (interactive)
  (picp-db-update))


;;; Picpocket mode commands

(defun picp-rotate-counter-clockwise (&optional arg)
  "Display the current picture rotated 90 degrees to the left.
This command do not change the picture file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picp-command
    (picp-rotate -90.0 arg)))

(defun picp-rotate-clockwise (&optional arg)
  "Display the current picture rotated 90 degrees to the right.
This command do not change the picture file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picp-command
    (picp-rotate 90.0 arg)))

(defun picp-rotate-counter-clockwise-10-degrees (&optional arg)
  "Display the current picture rotated 10 degrees to the left.
This command do not change the picture file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picp-command
    (picp-rotate -10.0 arg)))

(defun picp-rotate-clockwise-10-degrees (&optional arg)
  "Display the current picture rotated 10 degrees to the right.
This command do not change the picture file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picp-command
    (picp-rotate 10.0 arg)))

(defun picp-reset-rotation ()
  "Display the current picture as is without any rotation."
  (interactive)
  (picp-command
    (picp-set-rotation picp-current 0.0)))

(defun picp-rotate (delta arg)
  (let ((degrees (if arg
                     (float (read-number "Set rotation in degrees"
                                         (picp-rotation)))
                   (+ (picp-rotation) delta))))
    (picp-error-if-rotation-is-unsupported)
    (picp-set-rotation picp-current degrees)
    (picp-old-update-buffer)))


(defun picp-scale-in (&optional arg)
  "Zoom in 10%.
With prefix arg (ARG) read scale percent in minibuffer."
  (interactive "P")
  (picp-command
    (if arg
        (setq picp-scale (read-number "Scale factor: " picp-scale))
      (picp-alter-scale 10))
    (picp-warn-if-scaling-is-unsupported)
    (picp-old-update-buffer)))

(defun picp-scale-out (&optional arg)
  "Zoom out 10%.
With prefix arg (ARG) read scale percent in minibuffer."
  (interactive "P")
  (picp-command
    (if arg
        (setq picp-scale (read-number "Scale factor: " picp-scale))
      (picp-alter-scale -10))
    (picp-warn-if-scaling-is-unsupported)
    (picp-old-update-buffer)))

(defun picp-reset-scale ()
  "Reset the scale to 100%."
  (interactive)
  (picp-command
    (setq picp-scale 100)
    (message "Restore the scale to 100%%.")
    (picp-old-update-buffer)))

(defun picp-fit-to-width-and-height ()
  "Fit the picture to both width and height of window.
Fitting is done before applying the scaling factor.  That is, it
will only really fit when the scaling is the default 100%.  The
scaling can be restored to 100% by typing \\[picp-reset-scale]
\(bound to the command `picp-reset-scale')."
  (interactive)
  (picp-command
    (setq picp-fit :x-and-y)
    (message "Fit picture to both width and height")
    (picp-warn-if-scaling-is-unsupported)
    (picp-old-update-buffer)))

(defun picp-fit-to-width ()
  "Fit the picture to the width of window.
Fitting is done before applying the scaling factor.  That is, it
will only really fit when the scaling is the default 100%.  The
scaling can be restored to 100% by typing \\[picp-reset-scale]
\(bound to the command `picp-reset-scale')."
  (interactive)
  (picp-command
    (setq picp-fit :x)
    (message "Fit picture to width")
    (picp-warn-if-scaling-is-unsupported)
    (picp-old-update-buffer)))

(defun picp-fit-to-height ()
  "Fit the picture to the height of window.
Fitting is done before applying the scaling factor.  That is, it
will only really fit when the scaling is the default 100%.  The
scaling can be restored to 100% by typing \\[picp-reset-scale]
\(bound to the command `picp-reset-scale')."
  (interactive)
  (picp-command
    (setq picp-fit :y)
    (message "Fit picture to height")
    (picp-warn-if-scaling-is-unsupported)
    (picp-old-update-buffer)))

(defun picp-no-fit ()
  "Do not fit the picture to the window."
  (interactive)
  (picp-command
    (setq picp-fit nil)
    (message "Do not fit picture to window size")
    (picp-old-update-buffer)))


(defun picp-set-backdrop ()
  "Attempt to install the current picture as desktop backdrop."
  (interactive)
  (picp-command
    (picp-ensure-current-pic)
    (setq picp-backdrop-command (or picp-backdrop-command
                                    (picp-default-backdrop-command)))
    (unless picp-backdrop-command
      (error (concat "Command to set backdrop not found."
                     " Set picp-backdrop-command or install %s")
             (picp-default-backdrop-commands-string)))
    (let* ((words (split-string picp-backdrop-command))
           (cmd (car words))
           (file (picp-absfile))
           (args (append (cdr words) (list file))))
      (with-temp-buffer
        (unless (zerop (apply #'call-process cmd nil t nil args))
          (error "Command \"%s %s\" failed with output \"%s\""
                 picp-backdrop-command file (buffer-string))))
      (message "%s installed %s as backdrop" cmd file))))

(defun picp-default-backdrop-command ()
  (cl-loop for (cmd . args) in picp-default-backdrop-commands
           when (executable-find cmd)
           return (concat cmd " " args " ")))


(defun picp-default-backdrop-commands-string ()
  (concat (mapconcat #'identity
                     (butlast (mapcar #'car picp-default-backdrop-commands))
                     ", ")
          " or "
          (caar (last picp-default-backdrop-commands))))

(defun picp-toggle-debug ()
  "Toggle debug mode."
  (interactive)
  (picp-command
    (setq picp-debug (not picp-debug))
    (message "Picpocket debug is %s." (if picp-debug "on" "off"))))

(defun picp-toggle-recursive ()
  "Toggle recursive inclusion of sub-directories.
Directories whose name starts with a dot will not be traversed.
However picture files whose name starts with dot will be
included."
  (interactive)
  (picp-command
    (setq picp-recursive (not picp-recursive))
    (picp-do-revert)
    (message (if picp-recursive
                 "Recursively include pictures in subdirectories."
               "Only show pictures in current directory."))))

(defun picp-toggle-follow-symlinks ()
  "Toggle whether to follow symlinks while recursively traversing directories.
Symlinks that points to picture files are always followed.
This command concerns only symlinks that points to directories."
  (interactive)
  (picp-command
    (setq picp-follow-symlinks (not picp-follow-symlinks))
    (picp-do-revert)
    (message (if picp-follow-symlinks
                 "Follow symlinks."
               "Do not follow symlinks."))))




(defun picp-quit ()
  "Quit picpocket."
  (interactive)
  (picp-bye-command
    (picp-disable-fullscreen)
    (picp-save-journal)
    (quit-window)))

(defun picp-disable-fullscreen ()
  (when (picp-fullscreen-p)
    (picp-toggle-fullscreen-frame)))

(defun picp-toggle-destination-dir (&optional ask-for-dir)
  "Toggle the destination for file operations.

File operations are move, copy and hardlink.  Either the
destination is relative to the current directory.  Or it is
relative the value of variable `picp-destination-dir'.

With prefix arg ask for a directory and set variable
`picp-destination-dir' to that.  Calling from Lisp with the argument
ASK-FOR-DIR non-nil will also do that."
  (interactive "P")
  (picp-command
    (if ask-for-dir
        (setq picp-destination-relative-current nil
              picp-destination-dir
              (read-directory-name "Destination dir: "))
      (setq picp-destination-relative-current
            (not picp-destination-relative-current)))
    (message "Destination directory is relative to %s."
             (if picp-destination-relative-current
                 "the current directory"
               picp-destination-dir))))



(defun picp-edit-keystrokes ()
  "Move to definition of variable `picp-keystroke-alist'.
To use this command you must set variable `picp-keystroke-alist'
to a variable symbol.  The purpose of this command is to be
able to quickly move to the definition and edit keystrokes."
  (interactive)
  (picp-bye-command
    (unless (and picp-keystroke-alist
                 (symbolp picp-keystroke-alist))
      (user-error (concat "You need to set picp-keystroke-alist "
                          "to a symbol for this command to work")))
    (find-variable-other-window picp-keystroke-alist)
    (goto-char (point-at-eol))))

(defun picp-slideshow ()
  "Start slide-show."
  (interactive)
  (picp-command
    (while (and (not (input-pending-p))
                (picp-next-pos))
      (picp-next)
      (when (picp-next-pos)
        (sit-for 8)))
    (message "End of slideshow.")))

(defun picp-visit-file ()
  "Open the current picture in default mode (normally `image-mode')."
  (interactive)
  (picp-bye-command
    (find-file (picp-absfile))))

(defun picp-toggle-fullscreen-frame ()
  "Toggle use of fullscreen frame.

The first call will show the picpocket buffer in a newly created
frame in fullscreen mode.  It is meant to only show the picpocket
buffer (but this is not enforced).  The second call will delete
this frame and go back to the old frame."
  (interactive)
  (picp-command
    (if (picp-fullscreen-p)
        (progn
          (delete-frame picp-frame)
          (setq picp-frame nil)
          (picp-select-frame picp-old-frame)
          (setq mode-line-format (default-value 'mode-line-format))
          (picp-old-update-buffer))
      (setq picp-old-frame (selected-frame)
            ;; Do not disable scroll bars and fringes, they are disabled
            ;; on buffer level instead.
            picp-frame (make-frame `((name . "picpocket")
                                     (menu-bar-lines . 0)
                                     (tool-bar-lines . 0)
                                     (minibuffer . nil)
                                     (fullscreen . fullboth)
                                     ;; PENDING - background-color seem
                                     ;; to mess up the cache.
                                     ;; See image.c:search_image_cache.
                                     ;; (foreground-color . "white")
                                     ;; (background-color . "black")
                                     )))
      (picp-select-frame picp-frame)
      (setq mode-line-format nil)
      (add-hook 'focus-in-hook #'picp-focus)
      (add-hook 'minibuffer-setup-hook #'picp-minibuffer-setup)
      (add-hook 'minibuffer-exit-hook #'picp-minibuffer-exit)

      ;; Resdisplay seem to be needed to get accurate return value from
      ;; window-inside-pixel-edges.
      (redisplay)
      (picp-old-update-buffer))))

(defvar picp-last-frame-that-used-minibuffer nil)

(defun picp-focus ()
  "Update picture when `picp-minibuffer-exit' select `picp-frame'.
Without this the picture size may be fitted to the wrong frame.
This hook make sure it is fitted to `picp-frame'."
  (and (eq (selected-frame) picp-frame)
       (eq (current-buffer) (get-buffer picp-buffer))
       (picp-update-buffer)))

(defun picp-minibuffer-setup ()
  (setq picp-last-frame-that-used-minibuffer last-event-frame)
  (when (eq picp-frame last-event-frame)
    (select-frame-set-input-focus default-minibuffer-frame)))

(defun picp-minibuffer-exit ()
  (when (and (picp-fullscreen-p)
             (eq picp-frame picp-last-frame-that-used-minibuffer))
    (select-frame-set-input-focus picp-frame)))

(defun picp-fullscreen-p ()
  (and picp-frame
       (frame-live-p picp-frame)))


(defun picp-select-frame (frame)
  (select-frame-set-input-focus frame)
  (switch-to-buffer picp-buffer)
  ;; set-window-buffer will update the fringes.
  (set-window-buffer (selected-window) (current-buffer)))


(defun picp-next ()
  "Move to the next picture in the current list."
  (interactive)
  (picp-command
    (let ((next (picp-next-pos)))
      (if next
          (picp-list-set-pos next)
        (picp-no-file "next")))
    (picp-old-update-buffer)))

(defun picp-next-pos (&optional pos)
  (unless pos
    (setq pos (picp-current-pos)))
  (cl-loop for pic = (cdr (picp-pos-current pos)) then (cdr pic)
           for index = (1+ (picp-pos-index pos)) then (1+ index)
           while pic
           when (picp-filter-match-p pic)
           return (make-picp-pos :current pic
                                 :index index
                                 :filter-index
                                 (when (picp-pos-filter-index pos)
                                   (1+ (picp-pos-filter-index pos))))))

(defun picp-current-pos ()
  (make-picp-pos :current picp-current
                 :index picp-index
                 :filter-index picp-filter-index))

(defun picp-next-pic ()
  (picp-when-let (pos (picp-next-pos))
    (picp-pos-current pos)))

(defun picp-previous ()
  "Move to the previous picture in the current list."
  (interactive)
  (picp-command
    (let ((prev (picp-previous-pos)))
      (if prev
          (picp-list-set-pos prev)
        (picp-no-file "previous")))
    (picp-old-update-buffer)))

(defun picp-previous-pic ()
  (picp-when-let (pos (picp-previous-pos))
    (picp-pos-current pos)))

(defun picp-previous-pos ()
  (cl-loop for pic = (picp-safe-prev picp-current) then (picp-safe-prev pic)
           for index = (1- picp-index) then (1- index)
           while pic
           when (picp-filter-match-p pic)
           return (make-picp-pos :current pic
                                 :index index
                                 :filter-index (when picp-filter-index
                                                 (1- picp-filter-index)))))

(defun picp-safe-prev (pic)
  (when pic
    (picp-prev pic)))

(defun picp-home ()
  "Move to the first picture in the current list."
  (interactive)
  (picp-command
    (picp-list-set-pos (picp-first-pos))
    (unless (picp-filter-match-p picp-current)
      (let ((next (picp-next-pos)))
        (if next
            (picp-list-set-pos next)
          (picp-no-file))))
    (picp-old-update-buffer)))

(defun picp-first-pos ()
  (make-picp-pos :current picp-list
                 :index 1
                 :filter-index (if (picp-filter-match-p picp-list)
                                   1
                                 0)))

(defun picp-end ()
  "Move to the last picture in the current list."
  (interactive)
  (picp-command
    (picp-list-set-pos (picp-last-pos))
    (unless (picp-filter-match-p picp-current)
      (let ((prev (picp-previous-pos)))
        (if prev
            (picp-list-set-pos prev)
          (picp-no-file))))
    (picp-old-update-buffer)))

(defun picp-last-pos ()
  (cl-loop for pic on picp-current
           for index = picp-index then (1+ index)
           when (null (cdr pic))
           return (make-picp-pos :current pic
                                 :index index)))

(defun picp-delete-file ()
  "Permanently delete the current picture file."
  (interactive)
  (picp-command
    (picp-ensure-current-pic)
    (when (or (not picp-confirm-delete)
              (picp-y-or-n-p "Delete file %s from disk? " (picp-file)))
      (picp-action 'delete nil)
      (picp-old-update-buffer))))

(defun picp-y-or-n-p (format &rest objects)
  (let* ((prompt (apply #'format format objects))
         (header-line-format (concat prompt " (y or n)")))
    (y-or-n-p prompt)))

(defun picp-repeat ()
  "Repeat the last repeatable action.
The repeatable actions are:
1. Move/copy/hardlink the current picture to a directory.
2. Add a tag to or remove a tag from the current picture."
  (interactive)
  (picp-command
    (unless picp-last-action
      (user-error "No repeatable action have been done"))
    (picp-action picp-last-action picp-last-arg)
    (picp-old-update-buffer)))

(defun picp-dired ()
  "Visit the current directory in `dired-mode'."
  (interactive)
  (picp-bye-command
    (if picp-current
        (let ((dir (picp-dir))
              (file (picp-absfile)))
          (dired default-directory)
          (when (and (equal dir (file-truename default-directory))
                     (file-exists-p file))
            (dired-goto-file file)))
      (dired default-directory))))

(defun picp-dired-up-directory ()
  "Enter Dired mode in the parent directory."
  (interactive)
  (picp-bye-command
    (let ((dir default-directory))
      (quit-window)
      (dired (file-name-directory (directory-file-name dir)))
      (dired-goto-file dir))))

(defun picp-toggle-header ()
  "Toggle the display of the header line."
  (interactive)
  (picp-command
    (setq picp-header (not picp-header)
          header-line-format (when picp-header
                               picp-header-line-format))
    (force-mode-line-update)))


(defun picp-revert ()
  "Revert the current picpocket buffer.
Update the current list of pictures.
When called from Lisp return the new picpocket buffer."
  (interactive)
  (picp-command
    (picp-do-revert)))

(defun picp-do-revert ()
  ;; Selected-file is the second arg to all possible
  ;; picp-entry-functions.
  (apply picp-entry-function (append picp-entry-args
                                     (when picp-current
                                       (list (picp-absfile))))))

(defun picp-rename (dst)
  "Edit the filename of current picture.
If only the filename is changed the picture will stay as the
current picture.  But if it is moved to another directory it will
be removed from the picpocket list.
When called from Lisp DST is the new absolute filename."
  (interactive (list (progn
                       (picp-ensure-current-pic)
                       (when (boundp 'ido-read-file-name-non-ido)
                         (add-to-list 'ido-read-file-name-non-ido
                                      #'picp-rename))
                       (read-file-name "To: " nil (picp-file) nil
                                       (picp-file)))))
  (picp-command
    (picp-action (if (file-directory-p dst)
                     'move
                   'rename)
                 dst)
    (picp-old-update-buffer)))


(defun picp-move (all dst)
  "Move current picture to another directory.
With prefix arg (ALL) move all pictures in the picpocket list.
The picture will also be removed from the picpocket list.
When called from Lisp DST is the destination directory."
  (interactive (picp-read-destination 'move))
  (picp-command
    (picp-action 'move dst all)
    (picp-old-update-buffer)))

(defun picp-copy (all dst)
  "Copy the current picture to another directory.
With prefix arg (ALL) copy all pictures in the current list.
When called from Lisp DST is the destination directory."
  (interactive (picp-read-destination 'copy))
  (picp-command
    (picp-action 'copy dst all)
    (picp-old-update-buffer)))

(defun picp-hardlink (all dst)
  "Make a hard link to the current picture in another directory.
With prefix arg (ALL) hard link all pictures in the current list.
When called from Lisp DST is the destination directory."
  (interactive (picp-read-destination 'hardlink))
  (picp-command
    (picp-action 'hardlink dst all)
    (picp-old-update-buffer)))

(defun picp-read-destination (action)
  (picp-ensure-current-pic)
  (list (when current-prefix-arg
          'all)
        (file-name-as-directory
         (read-directory-name (format "%s%s to: "
                                      (capitalize (symbol-name action))
                                      (if current-prefix-arg
                                          " all pictures"
                                        ""))
                              (picp-destination-dir)))))

(defun picp-ensure-current-pic ()
  (unless (picp-filter-match-p picp-current)
    (user-error "No current picture"))
  (unless (file-exists-p (picp-absfile))
    (error "File %s no longer exists" (picp-file))))

(defun picp-destination-dir ()
  (if picp-destination-relative-current
      default-directory
    picp-destination-dir))

(defun picp-move-by-keystroke (all)
  "Move the current picture.
The destination directory is determined by a keystroke that is
lookup up in the variable `picp-keystroke-alist'.
With prefix arg (ALL) move all pictures in the current list."
  (interactive "P")
  (picp-command
    (picp-file-by-keystroke-command 'move all)))

(defun picp-copy-by-keystroke (all)
  "Copy the current picture.
The destination directory is determined by a keystroke that is
lookup up in the variable `picp-keystroke-alist'.
With prefix arg (ALL) copy all pictures in the current list."
  (interactive "P")
  (picp-command
    (picp-file-by-keystroke-command 'copy all)))

(defun picp-hardlink-by-keystroke (all)
  "Make a hard link to the current picture.
The destination directory is determined by a keystroke that is
lookup up in the variable `picp-keystroke-alist'.
With prefix arg (ALL) hard link all pictures in the current list."
  (interactive "P")
  (picp-command
    (picp-file-by-keystroke-command 'hardlink all)))

(defun picp-file-by-keystroke-command (action all)
  (picp-ensure-current-pic)
  (let ((prompt (format "directory to %s%s to"
                        (symbol-name action)
                        (if all " all pictures" ""))))
    (picp-action action
                 (picp-read-key prompt)
                 (when all 'all))
    (picp-old-update-buffer)))


(defun picp-tag-by-keystroke (&optional all)
  "Add a tag to the current picture.
The tag is determined by a keystroke that is looked up in the
variable `picp-keystroke-alist'.

With prefix arg (ALL) the tag is added to all pictures in the
current list.  Type a minus sign (-) before the keystroke to
remove the tag from all pictures instead."
  (interactive "P")
  (picp-command
    (picp-ensure-current-pic)
    (pcase (picp-read-key-to-add-or-remove-tag nil all)
      (`(,remove ,tag)
       (picp-action (if remove 'remove-tag 'add-tag)
                    tag
                    (if all 'all))))
    (picp-old-update-buffer)))

(defun picp-edit-tags (&optional all tags-string)
  "Edit the tags associated with current picture.
To enter multiple tags separate them with spaces.

With prefix arg (ALL) enter a single tag and add it to all
pictures in the current list.  If TAGS-STRING begins with a minus
sign (-) then the tag is removed from all pictures instead."
  (interactive "P")
  (picp-command
    (if all
        (picp-tag-to-all (or tags-string
                             (picp-read-tag-for-all)))
      (picp-ensure-current-pic)
      (let* ((old-tags-string (picp-tags-string-to-edit (picp-tags)))
             (new-tags-string (or tags-string
                                  (picp-read-tags "Tags: "
                                                  old-tags-string))))
        (picp-action 'set-tags new-tags-string)))
    (picp-old-update-buffer)))

(defun picp-read-tag-for-all ()
  (picp-read-tags "Type tag to add to all files (-tag to remove): "))

(defun picp-tag-to-all (tag-string)
  "Add a tag (TAG-STRING) to all pictures in current picpocket buffer.
If tag starts with minus remove tag instead of add."
  (cond ((string-match "\\`[:space:]*\\'" tag-string)
         (message "Empty string, no action."))
        ((not (eq 1 (length (split-string tag-string))))
         (message "Cannot handle more than one tag at a time"))
        ((eq (elt tag-string 0) ?-)
         (let ((tag (substring tag-string 1)))
           (picp-action 'remove-tag tag 'all)
           (message "Tag %s was removed from all." tag)))
        (t
         (picp-action 'add-tag tag-string 'all)
         (message "All tagged with %s." tag-string))))

(defun picp-read-tags (prompt &optional old-tags-string)
  (let ((string (minibuffer-with-setup-hook
                    (lambda ()
                      (setq-local completion-at-point-functions
                                  (list #'picp-tag-completion-at-point)))
                  (read-from-minibuffer prompt
                                        old-tags-string
                                        picp-minibuffer-map))))
    (dolist (tag (split-string string))
      (intern (string-remove-prefix "-" tag)
              picp-tag-completion-table))
    string))

(defun picp-tag-completion-at-point ()
  (list (save-excursion
          (skip-chars-backward "^ ")
          (skip-chars-forward "-")
          (point))
        (point)
        picp-tag-completion-table))

(defun picp-tags-string-to-edit (tags)
  (when tags
    (concat (mapconcat #'symbol-name tags " ") " ")))

(defun picp-set-filter (filter-string)
  "Enter the current picpocket filter.
The filter is a list of tags.  Only pictures with all the tags in
the filter is shown.  To enter multiple tags separate them with
spaces.

If `picp-filter-consider-dir-as-tag' is non-nil also the
containing directory counts as a tag as far as the filter is
concerned.

When called from Lisp the argument FILTER-STRING is a
space-separated string."
  (interactive (list (picp-read-tags
                      "Show only pictures with these tags: ")))
  (picp-command
    (picp-do-set-filter (mapcar #'intern (split-string filter-string)))
    (if picp-filter
        (message "Filter is %s" (picp-format-tags picp-filter))
      (message "No filter"))
    (picp-old-update-buffer)))

(defun picp-do-set-filter (filter)
  (setq picp-filter filter)
  (picp-reset-filter-counters))

(defun picp-reset-filter-counters ()
  (setq picp-filter-index nil
        picp-compute-filter-index-from-scratch t
        picp-filter-match-count nil
        picp-filter-match-count-done nil))

(defun picp-filter-match-p (pic)
  (cl-subsetp (picp-remove-hyphens picp-filter)
              (picp-remove-hyphens (append (picp-tags pic)
                                           (picp-extra-tags-for-filter pic)))))

(defun picp-remove-hyphens (list-or-symbol)
  (cond ((not picp-filter-ignore-hyphens)
         list-or-symbol)
        ((consp list-or-symbol)
         (mapcar #'picp-remove-hyphens list-or-symbol))
        (t (let ((str (symbol-name list-or-symbol)))
             (if (string-match "[-_]" str)
                 (let ((chars (string-to-list str)))
                   (intern (apply #'string (delete ?- (delete ?_ chars)))))
               list-or-symbol)))))

(defun picp-extra-tags-for-filter (pic)
  (mapcar #'intern
          (pcase picp-filter-consider-dir-as-tag
            (`nil nil)
            (`all (split-string (picp-dir pic) "/" t))
            (_ (list (file-name-nondirectory
                      (directory-file-name (picp-dir pic))))))))

(defun picp-no-file (&optional direction)
  (user-error (picp-join "No"
                         direction
                         "file"
                         (when picp-filter
                           (format "match filter %s" picp-filter)))))

(defun picp-set-filter-by-keystroke ()
  "Show only pictures having the tag in the current filter."
  (interactive)
  (picp-command
    (picp-set-filter (picp-read-key "filtering tag"))))


(defun picp-jump ()
  "Jump to picture specified by file-name or index number."
  (interactive)
  (picp-command
    (let ((nr-or-file-name (completing-read "Jump to index or file-name: "
                                            (picp-mapcar 'picp-file))))
      (or (picp-jump-to-index nr-or-file-name)
          (picp-jump-to-file nr-or-file-name))
      (picp-old-update-buffer))))

(defun picp-jump-to-index (string)
  (when (string-match "^[0-9]+$" string)
    (let ((index (string-to-number string)))
      (picp-when-let (pic (picp-pic-by-index index))
        (picp-list-set-pos (make-picp-pos :current pic
                                     :index index))
        t))))


(defun picp-pic-by-index (n)
  (and (< 0 n)
       (<= n picp-list-length)
       (cl-loop for pic on picp-list
                repeat (1- n)
                finally return pic)))

(defun picp-jump-to-file (file)
  (let ((pos-list (picp-pos-list-by-file file)))
    (cond ((null pos-list)
           (user-error "Picture not found (%s)" file))
          ((eq 1 (length pos-list))
           (picp-list-set-pos (car pos-list)))
          (t
           (let ((prompt (format "%s is available in %s directories.  Select: "
                                 file (length pos-list))))
             (picp-list-set-pos (picp-select-pos-by-dir pos-list prompt))
             t)))))

(defun picp-pos-list-by-file (file)
  (cl-loop for pic on picp-list
           for index = 1 then (1+ index)
           when (equal (picp-file pic) file)
           collect (make-picp-pos :current pic
                                  :index index)))

(defun picp-select-pos-by-dir (pos-list prompt)
  (let* ((dirs (cl-loop for pos in pos-list
                        collect (picp-dir (picp-pos-current pos))))
         (dir (completing-read prompt dirs nil t)))
    (cl-loop for pos in pos-list
             when (equal dir (picp-dir (picp-pos-current pos)))
             return pos)))


(defun picp-gimp-open ()
  "Run gimp on the current picture."
  (interactive)
  (and picp-gimp-executable
       (not (file-name-absolute-p picp-gimp-executable))
       (setq picp-gimp-executable (executable-find picp-gimp-executable)))
  (unless (picp-absfile)
    (user-error "No current picture"))
  (start-process picp-gimp-executable
                 nil
                 picp-gimp-executable
                 (picp-absfile)))


;;; Pic double-linked list functions

;; These will call tag handling functions.

(defun picp-absfile (&optional pic)
  (unless pic
    (setq pic picp-current))
  (concat (picp-dir pic) (picp-file pic)))

(defun picp-set-absfile (pic absfile)
  (picp-set-file pic (file-name-nondirectory absfile))
  (picp-set-dir pic (file-name-directory absfile)))

(defun picp-make-pic (path)
  (make-picp-pic :dir (file-truename (file-name-directory path))
                 :file (file-name-nondirectory path)))

(defun picp-list-reset ()
  (setq picp-list nil
        picp-list-length 0)
  (picp-list-set-pos (make-picp-pos))
  (picp-do-set-filter nil))

(defun picp-list-set-pos (pos)
  (let ((inhibit-quit t))
    (setq picp-current (picp-pos-current pos)
          picp-index (or (picp-pos-index pos)
                         (picp-calculate-index picp-current))
          picp-filter-index (picp-pos-filter-index pos)
          picp-compute-filter-index-from-scratch (null picp-filter-index))))

(defun picp-mapc (f)
  (cl-loop for pic on picp-list
           when (picp-filter-match-p pic)
           do (funcall f pic)))

(defun picp-mapcar (f)
  (cl-loop for pic on picp-list
           when (picp-filter-match-p pic)
           collect (funcall f pic)))

(defun picp-list-search (absfile)
  (cl-loop for pic on picp-list
           when (equal absfile (picp-absfile pic))
           return pic))

(defun picp-calculate-index (&optional current)
  (cl-loop for pic on picp-list
           count 1
           until (eq pic (or current picp-current))))

(defun picp-list-delete (&optional pic filter-match-cell)
  (setq pic (or pic
                picp-current))
  (let ((filter-match (if filter-match-cell
                          (car filter-match-cell)
                        (picp-filter-match-p pic))))
    (clear-image-cache (picp-absfile pic))
    (setq picp-list-length (1- picp-list-length))
    (if (picp-prev pic)
        (setcdr (picp-prev pic) (cdr pic))
      (setq picp-list (cdr pic)))
    (if (cdr pic)
        (progn
          (picp-set-prev (cdr pic) (picp-prev pic))
          (when (eq picp-current pic)
            (picp-list-set-pos (make-picp-pos :current (cdr pic)
                                              :index picp-index))))
      (if (picp-prev pic)
          (when (eq picp-current pic)
            (picp-list-set-pos (make-picp-pos :current (picp-prev pic)
                                              :index (1- picp-index))))
        (picp-list-reset)))
    (and picp-filter
         filter-match
         (if picp-filter-match-count-done
             (cl-decf picp-filter-match-count)
           (setq picp-filter-match-count nil)))))


;; (defun picp-list-insert-after-current (p)
;; "Insert P after current pic."
;; (let ((inhibit-quit t)
;; (prev picp-current)
;; (next (cdr picp-current)))
;; (setq picp-list-length (1+ picp-list-length))
;; (setf (picp-pic-prev p) prev)
;; (picp-list-set-pos (make-picp-pos :current (cons p next)
;; :index (1+ picp-index)))
;; (if prev
;; (setcdr prev picp-current)
;; (setq picp-list picp-current))
;; (when next
;; (picp-set-prev next picp-current))
;; (and picp-filter
;; (picp-filter-match-p picp-current)
;; (cl-incf picp-filter-match-count))))

(defun picp-list-insert-before-current (p)
  "Insert P before current pic."
  (let ((inhibit-quit t)
        (prev (and picp-current
                   (picp-prev picp-current)))
        (next picp-current))
    (setq picp-list-length (1+ picp-list-length))
    (setf (picp-pic-prev p) prev)
    (picp-list-set-pos (make-picp-pos :current (cons p next)
                                      :index (max 1 picp-index)))
    (if prev
        (setcdr prev picp-current)
      (setq picp-list picp-current))
    (when next
      (picp-set-prev next picp-current))
    (and picp-filter
         (picp-filter-match-p picp-current)
         (cl-incf picp-filter-match-count))))


(defun picp-create-picp-list (files &optional selected-file)
  (picp-list-reset)
  (setq picp-list
        (cl-loop for path in files
                 if (file-exists-p path)
                 collect (picp-make-pic path)
                 else do (message "%s do not exist" path)))
  (cl-loop for pic on picp-list
           with prev = nil
           do (picp-set-prev pic prev)
           do (setq prev pic))
  (setq picp-list-length (length picp-list))
  (picp-list-set-pos (or (and selected-file
                         (string-match (picp-picture-regexp) selected-file)
                         (cl-loop for pic on picp-list
                                  for index = 1 then (1+ index)
                                  when (equal selected-file (picp-absfile pic))
                                  return (make-picp-pos :current pic
                                                        :index index)))
                    (make-picp-pos :current picp-list
                                   :index 1))))



(defvar picp-done-dirs nil)
(defvar picp-file-count 0)

(defun picp-file-list (dir)
  (let ((picp-file-count 0)
        (picp-done-dirs nil))
    (prog1
        (picp-file-list2 (directory-file-name (file-truename dir)))
      (message "Found %s pictures" picp-file-count))))

(defun picp-file-list2 (dir)
  (push dir picp-done-dirs)
  (condition-case err
      (let ((files (picp-sort-files (directory-files dir nil "[^.]" t)))
            path pic-files sub-files subdirs)
        (dolist (file files)
          (setq path (expand-file-name file dir))
          (if (file-directory-p path)
              (push path subdirs)
            (when (string-match (picp-picture-regexp) file)
              (push path pic-files)
              (when (zerop (% (cl-incf picp-file-count) 100))
                (message "Found %s pictures so far %s"
                         picp-file-count
                         (if picp-recursive
                             (format "(%s)" dir)
                           ""))))))
        (setq pic-files (nreverse pic-files))
        (when picp-recursive
          (dolist (subdir subdirs)
            (when (or picp-follow-symlinks
                      (not (file-symlink-p subdir)))
              (let ((true-subdir (directory-file-name
                                  (file-truename subdir))))
                (unless (or (picp-dot-file-p subdir)
                            (member true-subdir picp-done-dirs))
                  (setq sub-files (append (picp-file-list2 true-subdir)
                                          sub-files)))))))
        (append pic-files sub-files))
    (file-error (progn
                  (warn "Failed to access %s (%s)" dir err)
                  nil))))

(defun picp-sort-files (files)
  (sort files #'picp-file-name-lessp))

(defun picp-file-name-lessp (a b)
  (cond ((and (equal "" a)
              (not (equal "" b)))
         t)
        ((equal "" b)
         nil)
        ((and (picp-string-start-with-number-p a)
              (picp-string-start-with-number-p b))
         (pcase-let ((`(,a-number . ,a-rest) (picp-parse-number a))
                     (`(,b-number . ,b-rest) (picp-parse-number b)))
           (if (= a-number b-number)
               (picp-file-name-lessp a-rest b-rest)
             (< a-number b-number))))
        (t (let ((a-char (aref a 0))
                 (b-char (aref b 0)))
             (if (= a-char b-char)
                 (picp-file-name-lessp (substring a 1) (substring b 1))
               (< a-char b-char))))))

(defun picp-parse-number (string)
  (picp-do-parse-number string ""))

(defun picp-do-parse-number (string acc)
  (if (picp-string-start-with-number-p string)
      (picp-do-parse-number (substring string 1)
                            (concat acc (substring string 0 1)))
    (cons (string-to-number acc) string)))

(defun picp-string-start-with-number-p (s)
  (unless (zerop (length s))
    (picp-char-is-number-p (aref s 0))))

(defun picp-char-is-number-p (c)
  (and (>= c ?0)
       (<= c ?9)))

;; (defun picp-parse-number (string)
;; (cl-loop for i from 0 to (1- (length string))
;; while (picp-char-is-number-p (aref string i))
;; finally return (cons (string-to-number (substring string 0 i))
;; (substring string i))))

(defun picp-dot-file-p (file)
  "Return t if the FILE's name start with a dot."
  (eq (elt (file-name-nondirectory file) 0) ?.))



;;; Tag database interface functions

;; This layer translates from struct picp-pic to a sha1 checksum.
;; This checksum is refered to as sha and is used as index in the
;; database below.

(defun picp-tags (&optional pic)
  (picp-db-tags (picp-sha-force pic)))

(defun picp-tags-set (pic new-tags)
  (let ((match-before (picp-filter-match-p pic)))
    (picp-db-tags-set (picp-sha-force pic)
                      (picp-absfile pic)
                      new-tags)
    (let ((match-after (picp-filter-match-p pic)))
      (when (picp-xor match-before match-after)
        (setq picp-compute-filter-index-from-scratch t
              picp-filter-index nil)
        (if picp-filter-match-count-done
            (cl-incf picp-filter-match-count (if match-after 1 -1))
          (setq picp-filter-match-count nil
                picp-filter-match-count-done nil))))))

(defun picp-xor (a b)
  (not (eq (not a) (not b))))


(defun picp-tags-move-file (pic old-file new-file)
  (picp-db-tags-move-file (picp-sha-force pic) old-file new-file))

(defun picp-tags-copy-file (pic new-file)
  (picp-db-tags-copy-file (picp-sha-force pic) new-file))

(defun picp-tags-delete-file (pic deleted-file)
  (picp-db-tags-delete-file (picp-sha-force pic) deleted-file))


(defun picp-sha-force (pic)
  "Return the sha1 checksum of PIC.
The checksum will be computed if not already available.
Also, if there is a matching entry in the database with tags
then the file of PIC will be added to that entry."
  (or (picp-sha pic)
      (picp-save-sha-in-pic-and-db pic)))


(defun picp-save-sha-in-pic-and-db (pic)
  (let* ((file (picp-absfile pic))
         (sha (picp-sha1sum file))
         (inhibit-quit t))
    (picp-set-sha pic sha)
    (picp-db-tags-add-file sha file)
    sha))


(defun picp-sha1sum (file)
  (if (or picp-sha1sum-executable
          (setq picp-sha1sum-executable (executable-find "sha1sum")))
      (with-temp-buffer
        (unless (zerop (call-process picp-sha1sum-executable nil t nil file))
          (error "Failed to compute sha for %s" file))
        (goto-char (point-min))
        (skip-chars-forward "0-9a-f")
        (unless (eq (point) 41)
          (error "Unrecognized output from sha1sum for %s" file))
        (buffer-substring (point-min) (point)))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (sha1 (current-buffer)))))

;;; Tag database internal functions

;; This layer translates from sha to data.  This layer knows about the
;; representation of the database entries.  The database maps from sha
;; to a plist with the following keys:
;;
;; :files - list of truename file-names with this sha.
;; :tags - list of tag symbols.
;;

(defmacro picp-with-db (sha var-list &rest body)
  "Convenience macro for tag database access.
SHA is the sha1sum of the picture to lookup.  VAR-LIST contains
one or more of the symbols plist, tags and files.  BODY will be
evaluated with the symbols in VAR-LIST bound to their values in
the database for the given SHA."
  (declare (indent 2)
           (debug (form form body)))
  (let ((invalid (cl-set-difference var-list '(plist files tags))))
    (when invalid
      (error "Invalid symbols in picp-with-db var-list: %s"
             invalid)))
  `(let* ((plist (picp-db-get ,sha))
          ,(if (memq 'files var-list)
               '(files (plist-get plist :files))
             'ignored)
          ,(if (memq 'tags var-list)
               '(tags (plist-get plist :tags))
             'ignored))
     ,@body))

(defun picp-db-tags (sha)
  (picp-with-db sha (tags)
    tags))

(defun picp-db-files (sha)
  (picp-with-db sha (files)
    files))

(defun picp-db-tags-add-file (sha file)
  (picp-with-db sha (plist files tags)
    (when tags
      (unless (member file files)
        (setq plist (plist-put plist :files (cons file files)))
        (picp-db-put sha plist)))))

(defun picp-db-tags-set (sha file new-tags)
  (picp-with-db sha (plist files)
    (if new-tags
        (picp-db-put sha (list :files (picp-add-to-list file files)
                               :tags new-tags))
      (when plist
        (picp-db-remove sha)))))

(defun picp-add-to-list (element list)
  (cl-adjoin element list :test 'equal))

(defun picp-db-tags-move-file (sha old-file new-file)
  (picp-with-db sha (plist files)
    (when plist
      (let ((new-files (picp-add-to-list new-file
                                         (delete old-file files))))
        (picp-db-put sha (plist-put plist :files new-files))))))

(defun picp-db-tags-copy-file (sha new-file)
  (picp-with-db sha (plist files)
    (when plist
      (unless (member new-file files)
        (picp-db-put sha (plist-put plist :files (cons new-file files)))))))

(defun picp-db-tags-delete-file (sha deleted-file)
  (picp-with-db sha (plist files)
    (when plist
      (let ((new-files (delete deleted-file files)))
        (if new-files
            (picp-db-put sha (plist-put plist :files new-files))
          (picp-db-remove sha))))))


;;; Tag database management

(defvar picp-db-mode-map nil)
(defvar picp-db nil)
(defvar picp-db-update-buffer "*picpocket-db-update*")

(defun picp-db-update ()
  (when (get-buffer picp-db-update-buffer)
    (kill-buffer picp-db-update-buffer))
  (with-current-buffer (get-buffer-create picp-db-update-buffer)
    (switch-to-buffer (current-buffer))
    (picp-do-db-update)))

(defun picp-do-db-update ()
  (let* ((alist (picp-db-traverse))
         (sha-changed (cdr (assq :sha-changed alist)))
         (unique-file-missing (cdr (assq :unique-file-missing alist)))
         (redundant-file-missing (cdr (assq :redundant-file-missing alist)))
         buffer-read-only)
    (buffer-disable-undo)
    (erase-buffer)
    (setq truncate-lines t)
    (insert "\n")

    (setq picp-db-mode-map (make-sparse-keymap))

    ;; exif -c -o x.jpg --ifd=EXIF -t0x9286 --set-value=foo x.jpg
    (if (null sha-changed)
        (picp-emph "No files with changed sha1 checksum found.\n\n")
      (let ((n (length sha-changed)))
        (picp-db-update-command [?s]
          (lambda ()
            (insert (picp-emph "The following %s file%s have "
                               n (picp-plural-s n))
                    (picp-emph "changed %s sha1 checksum.\n"
                               (picp-plural-its-their n))
                    "Type "
                    (picp-emph "s")
                    " to update picpocket database with the new"
                    " sha1 checksum values.\n\n")
            (picp-insert-file-list sha-changed))
          (lambda ()
            (picp-update-sha sha-changed)
            (insert (picp-emph "Sha1 checksums for %s file%s were updated.\n"
                               n (picp-plural-s n)))))))

    (if (null redundant-file-missing)
        (insert (picp-emph "No missing redundant files.\n\n"))
      (let ((n (length redundant-file-missing)))
        (picp-db-update-command [?r]
          (lambda ()
            (insert (picp-emph "The following %s redundant file name%s"
                               n (picp-plural-s n))
                    (picp-emph " were not found on disk.\n")
                    "Their database entries contains at least one other"
                    " file that do exist.\n"
                    "Type "
                    (picp-emph "r")
                    " to remove these file names from the picpocket database.\n"
                    "(Their database entries will not be removed.)\n\n")
            (picp-insert-file-list redundant-file-missing))
          (lambda ()
            (picp-remove-file-names-in-db redundant-file-missing)
            (insert (picp-emph "Removed %s missing "
                               n)
                    (picp-emph "redundant file name%s from database."
                               (picp-plural-s n)))))))

    (if (null unique-file-missing)
        (insert (picp-emph "No missing unique files.\n\n"))
      (let ((n (length unique-file-missing)))
        (picp-db-update-command [?u]
          (lambda ()
            (insert (picp-emph "The following %s unique file name%s"
                               n (picp-plural-s n))
                    (picp-emph " were not found on disk.\n")
                    "Their database entries do not contain any"
                    " existing files.\n"
                    "Type "
                    (picp-emph "u")
                    " to remove these entries from the picpocket database.\n"
                    "(Their database entries will be removed.)\n\n")
            (picp-insert-file-list unique-file-missing))
          (lambda ()
            (picp-remove-file-names-in-db unique-file-missing)
            (insert (picp-emph "Removed %s missing unique file name%s"
                               n (picp-plural-s n))
                    (picp-emph " and their entries from database."))))))

    (goto-char (point-min))
    (picp-db-mode)))

(defun picp-db-update-command (key text-function command-function)
  "Create a command and bind it to KEY.

The TEXT-FUNCTION will be called immediately and is supposed to
insert some text describing what COMMAND-FUNCTION does.  When KEY
is typed that text will be deleted and the COMMAND-FUNCTION will
be called.  COMMAND-FUNCTION may also insert some text and that
will end up replacing the deleted text."
  (declare (indent defun))
  (let ((start (point)))
    (funcall text-function)
    (insert "\n")
    (let ((overlay (make-overlay start (1- (point)))))
      (define-key picp-db-mode-map key
        (lambda ()
          (interactive)
          (if (null (overlay-start overlay))
              (message "Nothing more to do.")
            (let (buffer-read-only)
              (goto-char (overlay-start overlay))
              (delete-region (overlay-start overlay)
                             (overlay-end overlay))
              (delete-overlay overlay)
              (funcall command-function)
              (insert "\n"))))))))


(defun picp-update-sha (sha-changed)
  (cl-loop for (file new-tags sha new-sha) in sha-changed
           do (picp-with-db new-sha (plist files tags)
                (picp-db-put new-sha (list :files (picp-add-to-list file
                                                                    files)
                                           :tags (cl-union tags
                                                           new-tags))))
           do (picp-with-db sha (plist files)
                (let ((remaining-files (delete file files)))
                  (if remaining-files
                      (picp-db-put sha (plist-put plist
                                                  :files
                                                  remaining-files))
                    (picp-db-remove sha))))))

(defun picp-remove-file-names-in-db (missing-files)
  (cl-loop for (file ignored sha) in missing-files
           do (picp-with-db sha (plist files)
                (let ((new-files (delete file files)))
                  (if new-files
                      (picp-db-put sha (plist-put plist
                                                  :files
                                                  new-files))
                    (picp-db-remove sha))))))

(defun picp-emph (format &rest args)
  (propertize (apply #'format format args)
              'face 'bold
              'font-lock-face 'bold))

(defun picp-insert-file-list (list)
  (dolist (entry list)
    (insert "  "
            (picp-join (car entry)
                       (picp-format-tags (cadr entry)))
            "\n")))

(define-derived-mode picp-db-mode special-mode "picpocket-db"
  (define-key picp-db-mode-map [?g] #'picpocket-db-update)
  (setq truncate-lines t))

(defun picp-db-traverse ()
  (picp-db-init)
  (let ((progress (make-progress-reporter "Traversing database "
                                          0
                                          (hash-table-count picp-db)))
        (i 0)
        sha-changed
        unique-file-missing
        redundant-file-missing)
    (maphash (lambda (sha plist)
               (let ((tags (plist-get plist :tags))
                     (files (plist-get plist :files))
                     missing-files existing-files)
                 (dolist (file files)
                   (if (file-exists-p file)
                       (let ((new-sha (picp-sha1sum file)))
                         (unless (equal sha new-sha)
                           (push (list file tags sha new-sha) sha-changed))
                         (push file existing-files))
                     (push (list file tags sha) missing-files)))
                 (when missing-files
                   (if existing-files
                       (setq redundant-file-missing
                             (append missing-files
                                     redundant-file-missing))
                     (setq unique-file-missing
                           (append missing-files
                                   unique-file-missing))))
                 (cl-incf i)
                 (progress-reporter-update progress i)))
             picp-db)
    (progress-reporter-done progress)
    (list (cons :sha-changed sha-changed)
          (cons :unique-file-missing unique-file-missing)
          (cons :redundant-file-missing redundant-file-missing))))

(defun picp-db-compile-tags-for-completion ()
  (maphash (lambda (_ plist)
             (dolist (tag (plist-get plist :tags))
               (intern (symbol-name tag)
                       picp-tag-completion-table)))
           picp-db))



;;; Database

;; This "database" stores a hash table in a text file.
;; The file format is:
;;
;; (version VERSION)
;; (format FORMAT)
;; (data-version DATA-VERSION)
;;
;; where VERSION is the integer version of the picp database and
;; DATA-VERSION is the integer version of the stored data.  FORMAT is
;; either list or hash-table.  In case of hash-table format the next
;; value is the hash table itself.  The hash table maps sha1 checksums
;; to data entries.  In case of list format lists like (SHA DATA)
;; follows for every hash table entry.
;;
;; TODO
;; SQL support (no, not really)
;; NoSQL support (don't we have that already?)


(defvar picp-db-dir (concat user-emacs-directory "picpocket/"))

(defconst picp-db-version 1)

(defvar picp-db nil)

(defvar picp-db-remove-corrupted-files nil)

(defvar picp-db-format 'list
  "Either `list' or `hash-table'.
`hash-table' is faster.
`list' makes the database files more readable.")

(defvar picp-db-valid-formats '(list hash-table))

(defvar picp-db-journal-size 0)


(defun picp-db-journal-size ()
  picp-db-journal-size)

(defun picp-db-get (sha)
  (gethash sha picp-db))

(defun picp-db-put (sha data)
  (let ((inhibit-quit t)
        (coding-system-for-write 'utf-8-unix)
        print-level print-length)
    (if data
        (puthash sha data picp-db)
      (remhash sha picp-db))
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (unless (file-exists-p (picp-db-file :journal))
          (picp-db-insert-header)
          (prin1 (list 'version picp-db-version))
          (insert "\n"))
        (picp-db-insert-list-item (list sha data))
        (write-region (point-min)
                      (point-max)
                      (picp-db-file :journal)
                      t
                      'silent)))
    (cl-incf picp-db-journal-size)))

(defun picp-db-insert-header ()
  (insert ";; -*- coding: utf-8-unix; no-byte-compile: t -*-\n")
  (insert ";; This file is auto-generated by picpocket.el in Emacs.\n")
  (insert ";; If you plan to manually edit this file you should first\n")
  (insert ";; kill the *picpocket* buffer in Emacs.  Otherwise your\n")
  (insert ";; edits may become overwritten.\n\n"))

(defun picp-db-insert-list-item (item)
  (prin1 item)
  (insert "\n"))

(defun picp-db-remove (sha)
  (picp-db-put sha nil))

(defun picp-db-clear ()
  (when (hash-table-p picp-db)
    (clrhash picp-db))
  (setq picp-db (picp-db-new-hash-table)))

(defun picp-db-count ()
  (hash-table-count picp-db))

(defun picp-db-init ()
  (make-directory picp-db-dir t)
  (let ((db (picp-db-read nil))
        (old (picp-db-read :old)))
    (setq picp-db
          (cond ((and (hash-table-p db) (null old))
                 db)
                ((and (null db) (null old))
                 (picp-db-new-hash-table))
                ((and (not (hash-table-p db)) (hash-table-p old))
                 (picp-warn "Recovering with picpocket old file")
                 old)
                ((and (hash-table-p db) (hash-table-p old))
                 (picp-warn "Ignoring spurious picpocket old file (%s)"
                            (picp-db-file :old))
                 (when picp-db-remove-corrupted-files
                   (delete-file (picp-db-file :old)))
                 db)
                ((and (hash-table-p db) (eq old 'error))
                 (picp-warn "Ignoring corrupt picpocket old file (%s)"
                            (picp-db-file :old))
                 (when picp-db-remove-corrupted-files
                   (delete-file (picp-db-file :old)))
                 db)
                (t
                 (message "(hash-table-p db) %s" (hash-table-p db))
                 (message "(hash-table-p old) %s" (hash-table-p old))
                 (message "db %s" db)
                 (message "old %s" old)
                 ;; PENDING - kill picpocket buffer?
                 (error "Cannot recover picpocket database"))))
    (when (file-exists-p (picp-db-file :journal))
      (picp-db-read-journal)
      (picp-db-save))))

(defun picp-db-new-hash-table ()
  (make-hash-table :test 'equal))

(defun picp-db-read (file-symbol)
  (let ((db-file (picp-db-file file-symbol)))
    (when (file-exists-p db-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents db-file)
            (let* ((standard-input (current-buffer))
                   (version (cadr (read)))
                   (format (cadr (read)))
                   (ignored (cadr (read))))
              (unless (equal version picp-db-version)
                (error "Unknown picpocket database version %s" version))
              (cl-case format
                (hash-table (picp-db-read-hash-table))
                (list (picp-db-read-list))
                (t (error "Unknown format %s in %s (%s)"
                          format db-file (picp-db-valid-formats-string))))))
        (error (picp-warn "Failed to read %s - %s" db-file err)
               'error)))))

(defun picp-db-read-hash-table ()
  (let ((db (read)))
    (unless (hash-table-p db)
      (error "Not a proper hash table"))
    db))

(defun picp-db-read-list ()
  (picp-db-read-and-hash-list (picp-db-new-hash-table)))

(defun picp-db-read-and-hash-list (hash-table &optional counter)
  (condition-case ignored
      (while t
        (cl-destructuring-bind (key value) (read)
          (if value
              (puthash key value hash-table)
            (remhash key hash-table))
          (when counter
            (set counter (1+ (symbol-value counter))))))
    (end-of-file))
  hash-table)


(defun picp-db-save ()
  (let ((db-file (picp-db-file))
        (tmp-file (picp-db-file :tmp))
        (old-file (picp-db-file :old))
        (journal-file (picp-db-file :journal)))
    (with-temp-file tmp-file
      (set-buffer-file-coding-system 'utf-8-unix)
      (let ((standard-output (current-buffer))
            (print-level print-length))
        (picp-db-insert-header)
        (prin1 (list 'version picp-db-version))
        (insert "\n")
        (prin1 (list 'format picp-db-format))
        (insert "\n")
        (prin1 (list 'data-version 1))
        (insert "\n")
        (cl-case picp-db-format
          (hash-table (picp-db-save-hash-table))
          (list (picp-db-save-list))
          (t (error "Unknown value of picp-db-format %s (%s)"
                    picp-db-format (picp-db-valid-formats-string))))
        (insert "\n")))
    (let ((inhibit-quit t))
      (when (file-exists-p db-file)
        (copy-file db-file old-file))
      (copy-file tmp-file db-file t)
      (delete-file tmp-file)
      (when (file-exists-p old-file)
        (delete-file old-file))
      (when (file-exists-p journal-file)
        (delete-file journal-file))
      (setq picp-db-journal-size 0))))




(defun picp-db-valid-formats-string ()
  (format "should be %s"
          (mapconcat #'symbol-name picp-db-valid-formats " or ")))


(defun picp-db-save-hash-table ()
  (prin1 picp-db)
  (insert "\n"))

(defun picp-db-dump ()
  (with-temp-buffer
    (picp-db-save-list)
    (buffer-string)))

(defun picp-db-save-list ()
  (let (list)
    (maphash (lambda (key value)
               (push (list key value) list))
             picp-db)
    ;; PENDING - optionally sort the list.
    (dolist (element list)
      (picp-db-insert-list-item element))))

(defun picp-db-file (&optional symbol)
  (concat picp-db-dir
          "picpocket-db"
          (when symbol
            (concat "-" (substring (symbol-name symbol) 1)))
          ".el"))

(defun picp-db-read-journal ()
  (setq picp-db-journal-size 0)
  (let ((journal-file (picp-db-file :journal)))
    (when (file-exists-p journal-file)
      (with-temp-buffer
        (insert-file-contents journal-file)
        (let* ((standard-input (current-buffer))
               (version (cadr (read))))
          (if (not (equal picp-db-version version))
              (picp-warn "Ignoring picpocket journal %s of unknown version %s"
                         journal-file version)
            (picp-db-read-and-hash-list picp-db 'picp-db-journal-size)))))))

(defun picp-warn (format &rest args)
  (let ((format (concat "picpocket-warn: " format)))
    (apply #'message format args)
    (unless picp-demote-warnings
      (apply #'warn format args))))



;;; Idle timer functions

(defvar picp-timers nil)
(defvar picp-idle-timer-work-functions
  '((picp-update-current-bytes 0.1)
    (picp-maybe-save-journal 0.2)
    (picp-look-ahead-next 0.2)
    ;; PENDING
    ;; (picp-look-ahead-more 2)
    (picp-compute-filter-index 0.5)
    (picp-compute-filter-match-count 1)
    (picp-traverse-pic-list 3)
    (picp-save-journal 60)
    (picp-update-header picp-update-header-seconds)))
(defvar picp-inhibit-timers nil)
(defvar picp-idle-timer-deadline 0.1)




(defun picp-init-timers ()
  (picp-cancel-timers)
  (unless picp-inhibit-timers
    (add-hook 'kill-buffer-hook #'picp-cancel-timers nil t)
    (setq picp-timers
          (cl-loop for (f s) in picp-idle-timer-work-functions
                   for sec = (if (functionp s)
                                 (funcall s)
                               s)
                   collect (run-with-idle-timer sec
                                                t
                                                #'picp-run-idle-timer
                                                f)))))

(defun picp-cancel-timers ()
  (dolist (timer picp-timers)
    (cancel-timer timer))
  (setq picp-timers nil)
  (dolist (ft picp-idle-timer-work-functions)
    (let* ((f (car ft))
           (resume-timer (get f 'picp-resume-timer)))
      (when resume-timer
        (cancel-timer resume-timer)))))


(defun picp-run-idle-timer (f &optional state)
  (let ((debug-on-error picp-debug-idle-timers)
        (resume-timer (get f 'picp-resume-timer))
        (buffer (get-buffer picp-buffer)))
    (when (timerp resume-timer)
      (cancel-timer resume-timer))
    (cond (picp-inhibit-timers
           (picp-cancel-timers))
          (buffer
           (with-current-buffer buffer
             (picp-run-idle-timer-in-buffer f state)))
          (t (picp-cancel-timers)))))

(defun picp-run-idle-timer-in-buffer (f state)
  (cond ((null picp-list)
         (picp-cancel-timers))
        ((null picp-db)
         (message "Cancel idle timers since picp-db is nil.")
         (picp-cancel-timers))
        ((not (file-directory-p default-directory))
         (message "Closing picpocket buffer since %s do not exist any more."
                  default-directory)
         (kill-buffer picp-buffer)
         (picp-cancel-timers))
        (t
         (condition-case err
             (funcall f (picp-make-deadline-function f) state)
           (quit (message "picp-run-idle-timer %s interrupted by quit" f)
                 (signal (car err) (cdr err)))))))

(defun picp-make-deadline-function (f)
  (let ((start (current-time)))
    (lambda (state)
      (when (time-less-p (seconds-to-time picp-idle-timer-deadline)
                         (time-subtract (current-time) start))
        (put f
             'picp-resume-timer
             (run-with-idle-timer (time-add (or (current-idle-time)
                                                (seconds-to-time 0))
                                            (seconds-to-time
                                             picp-idle-timer-deadline))
                                  nil
                                  #'picp-run-idle-timer
                                  f
                                  state))))))


(defun picp-traverse-pic-list (deadline-function state)
  (with-current-buffer picp-buffer
    (cl-loop for pic on (picp-continue-or-start-over state)
             do (picp-ensure-cache pic)
             until (funcall deadline-function pic))
    ;; picp-size-force may push out the very next pic from the
    ;; emacs image cache.  Call picp-look-ahead-next to put it back if
    ;; needed.
    (picp-look-ahead-next)))

(defun picp-ensure-cache (pic)
  (when (file-exists-p (picp-absfile pic))
    (picp-sha-force pic)
    (picp-size-force pic)
    (picp-bytes-force pic)))

(defun picp-continue-or-start-over (state)
  "Continue from saved STATE or start from scratch if STATE is invalid.
STATE is the last pic the idle timer worked on.  If that for
example have been deleted from the picture list then the state is
considered invalid and we start from the beginning again."
  (or (and state
           (memq state picp-list))
      picp-list))

(defun picp-compute-filter-match-count (deadline-function state)
  (with-current-buffer picp-buffer
    (unless picp-filter-match-count
      (setq picp-filter-match-count-done nil))
    (when (and picp-filter
               (not picp-filter-match-count-done))
      (when (or (null picp-filter-match-count)
                (and state
                     (not (memq state picp-list))))
        (setq state picp-list
              picp-filter-match-count 0))
      (unless (cl-loop for pic on state
                       do (when (picp-filter-match-p pic)
                            (cl-incf picp-filter-match-count))
                       when (funcall deadline-function pic)
                       return t)
        (setq picp-filter-match-count-done t)))))

(defun picp-compute-filter-index (deadline-function state)
  (with-current-buffer picp-buffer
    (when (and picp-filter
               (null picp-filter-index))
      (when (or (null state)
                picp-compute-filter-index-from-scratch
                (not (memq state picp-list)))
        (setq state (picp-first-pos)))
      (cl-loop for pos = state then (picp-next-pos pos)
               while pos
               when (eq (picp-pos-current pos) picp-current)
               return (setq picp-filter-index (picp-pos-filter-index pos))
               until (funcall deadline-function pos)))))


(defun picp-update-current-bytes (&rest ignored)
  (let ((bytes (picp-bytes)))
    (picp-bytes-force picp-current)
    (unless bytes
      (force-mode-line-update))))

(defun picp-maybe-save-journal (&rest ignored)
  (when (> (picp-db-journal-size) 100)
    (picp-db-save)))

(defun picp-save-journal (&rest ignored)
  (unless (zerop (picp-db-journal-size))
    (picp-db-save)))

(defun picp-look-ahead-next (&rest ignored)
  (let ((pic (or (picp-next-pic) (picp-previous-pic))))
    (when (and pic
               (not (eq pic picp-last-look-ahead)))
      (picp-look-ahead-and-save-time pic)
      (setq picp-last-look-ahead pic))))


(defun picp-look-ahead-more (deadline-function ignored)
  (let ((s (cadr (picp-time (picp-look-ahead-more2 deadline-function)))))
    (picp-debug s "more")))

(defun picp-look-ahead-more2 (deadline-function)
  (cl-loop for pic on picp-current
           for count = 0 then (1+ count)
           until (funcall deadline-function nil)
           finally return count
           repeat picp-look-ahead-max
           do (picp-look-ahead-and-save-time pic)))


;;; Buffer content functions

;; PENDING - Replaced by picp-command.....to be removed.....
(defun picp-old-update-buffer ())

(defun picp-ensure-picpocket-buffer ()
  (unless (and (equal (buffer-name) picp-buffer)
               (eq major-mode 'picp-mode))
    (message "buffer %s, mode %s" (buffer-name) major-mode)
    (error "%s requires picpocket mode" (or this-command
                                            "This"))))


(defun picp-update-buffer ()
  (let ((s (cadr (picp-time (picp-do-update-buffer)))))
    (picp-debug s "%s %s" this-command picp-index)))

(defun picp-do-update-buffer ()
  (picp-ensure-picpocket-buffer)
  (let (buffer-read-only)
    (erase-buffer)
    (if (picp-try-set-matching-picture)
        (progn
          (cd (picp-dir))
          (if (file-exists-p (picp-absfile))
              (picp-insert picp-current)
            (insert "\n\nFile " (picp-file) " no longer exist.\n")))
      (picp-no-pictures))
    (force-mode-line-update)))

(defun picp-try-set-matching-picture ()
  "Return nil if no matching picture was found."
  (when picp-current
    (or (picp-filter-match-p picp-current)
        (picp-when-let (pos (or (picp-next-pos) (picp-previous-pos)))
          (picp-list-set-pos pos)
          t))))

(defun picp-no-pictures ()
  (insert (propertize (format "\n\nNo pictures in list%s.\n\n"
                              (if picp-filter
                                  (format " matching filter %s" picp-filter)
                                ""))
                      'face 'bold))
  (when picp-filter
    (insert (format "Type %s to edit filter.\n"
                    (picp-where-is 'picp-set-filter))))
  (and (eq picp-entry-function 'picpocket-dir)
       (not picp-recursive)
       (insert
        (format "Type %s to recursively include pictures in subdirectories.\n"
                (picp-where-is 'picp-toggle-recursive))))
  (insert (format "Type %s for dired in %s.\n"
                  (picp-where-is 'picp-dired)
                  (abbreviate-file-name default-directory))))

(defun picp-where-is (command)
  (let ((binding (where-is-internal command overriding-local-map t)))
    (propertize (if binding
                    (key-description binding)
                  (concat "M-x " (symbol-name command)))
                'face 'bold)))



(defun picp-create-buffer (files &optional selected-file dir)
  (when selected-file
    (setq selected-file (file-truename
                         (expand-file-name selected-file dir))))
  (let ((old-buffer (get-buffer picp-buffer)))
    (when old-buffer
      (kill-buffer old-buffer)))
  (with-current-buffer (get-buffer-create picp-buffer)
    (when dir
      (cd dir))
    (picp-mode)
    (condition-case err
        (picp-create-picp-list files selected-file)
      (quit (picp-list-reset)
            (signal (car err) (cdr err))))
    (picp-update-buffer)
    (if (called-interactively-p 'any)
        (switch-to-buffer (current-buffer))
      (set-buffer (current-buffer)))
    (current-buffer))
  (set-buffer picp-buffer))


;;; Image handling

(defun picp-insert (pic)
  (if (display-images-p)
      (insert-image (picp-create-image pic (picp-save-window-size)))
    (insert "\n\nThis display does not support images."))
  (goto-char (point-min)))

(defun picp-save-window-size ()
  "Save the current window size.
This is for the benefit of timer functions that do not
necessarily run with the picpocket window selected."
  (setq picp-window-size (picp-current-window-size)))

(defun picp-current-window-size ()
  (cl-destructuring-bind (x0 y0 x1 y1) (window-inside-pixel-edges)
    ;; For some reason Emacs 25.0 refuses to draw image in a right
    ;; margin that seem to be (frame-char-width) pixels wide.
    ;; Therefore subtract that.
    (cons (- x1 x0 (frame-char-width))
          (- y1 y0))))

(defun picp-create-image (pic canvas-size)
  (pcase-let ((`(,keyword . ,value) (picp-clock
                                     (picp-size-param pic canvas-size))))
    (create-image (picp-absfile pic)
                  (picp-image-type pic)
                  nil
                  :rotation (picp-rotation pic)
                  keyword (picp-scale value))))

(defun picp-image-type (pic-or-filename)
  (let ((filename (if (stringp pic-or-filename)
                      pic-or-filename
                    (picp-file pic-or-filename))))
    (unless (string-suffix-p ".svg" filename t)
      'imagemagick)))

(defun picp-error-if-rotation-is-unsupported ()
  (unless (eq (picp-image-type picp-current) 'imagemagick)
    (error "Svg images cannot be rotated")))

(defun picp-warn-if-scaling-is-unsupported ()
  (unless (eq (picp-image-type picp-current) 'imagemagick)
    (message "Svg images cannot be scaled")))


(defun picp-size-param (pic canvas-size)
  (pcase-let ((canvas-ratio (picp-cons-ratio canvas-size))
              (rot-ratio (picp-cons-ratio (picp-rotated-size pic)))
              (`(,pic-x . ,_) (picp-size-force pic)))
    (pcase picp-fit
      (:x-and-y (if (> canvas-ratio rot-ratio)
                    (picp-height-size-param pic canvas-size)
                  (picp-width-size-param pic canvas-size)))
      (:x (picp-width-size-param pic canvas-size))
      (:y (picp-height-size-param pic canvas-size))
      (_ (cons :width pic-x)))))

(defun picp-height-size-param (pic canvas-size)
  (pcase-let* ((`(,_ . ,canvas-y) canvas-size)
               (`(,_ . ,pic-y) (picp-size-force pic))
               (`(,_ . ,rot-y) (picp-rotated-size pic))
               (y-ratio (picp-ratio pic-y rot-y)))
    (cons :height (round (* y-ratio canvas-y)))))

(defun picp-width-size-param (pic canvas-size)
  (pcase-let* ((`(,canvas-x . ,_) canvas-size)
               (`(,pic-x . ,_) (picp-size-force pic))
               (`(,rot-x . ,_) (picp-rotated-size pic))
               (x-ratio (picp-ratio pic-x rot-x)))
    (cons :width (round (* x-ratio canvas-x)))))

(defun picp-size-force (pic)
  (or (picp-size pic)
      (picp-save-size-in-pic pic)))

(defun picp-save-size-in-pic (pic)
  (picp-set-size pic (image-size (create-image (picp-absfile pic)
                                               (picp-image-type pic)
                                               nil
                                               :rotation 0.0)
                                 t)))

(defun picp-rotated-size (pic)
  (if (or (zerop (picp-rotation pic))
          (not (eq 'imagemagick (picp-image-type pic))))
      (picp-size-force pic)
    (image-size (create-image (picp-absfile pic)
                              (picp-image-type pic)
                              nil
                              :rotation (picp-rotation pic))
                t)))

(cl-defun picp-cons-ratio ((a . b))
  (/ (float a) b))

(defun picp-ratio (a b)
  (/ (float a) b))

(defun picp-look-ahead-and-save-time (pic)
  (let ((s (cadr (picp-time (picp-look-ahead pic))))
        (picp-sum 0))
    (picp-debug s "look")))

(defun picp-look-ahead (pic)
  (picp-ensure-cache pic)
  (image-size (picp-create-image pic picp-window-size)
              t
              (if (picp-fullscreen-p)
                  picp-frame
                (selected-frame))))

(defun picp-scale (n)
  (/ (* picp-scale n) 100))

(defun picp-alter-scale (delta)
  (setq picp-scale
        (max 10 (+ picp-scale delta)))
  (message "Scaling factor is %s%%" picp-scale))

(defun picp-imagemagick-p ()
  (picp-picture-regexp))

(defun picp-picture-regexp ()
  (or picp-picture-regexp
      (setq picp-picture-regexp (car (rassq 'imagemagick
                                            image-type-file-name-regexps)))))


(defun picp-clear-image-cache ()
  "Clear image cache.  Only useful for benchmarks."
  (interactive)
  (picp-command
    (setq picp-sum 0)
    (message "Clear image cache %s"
             (picp-time-string (clear-image-cache t)))))


;;; English functions

(defun picp-plural-s (n)
  (if (eq n 1)
      ""
    "s"))

(defun picp-plural-its-their (n)
  (if (eq n 1)
      "its"
    "their"))



;;; Keystroke and keymap functions

(defun picp-read-key (what)
  (let* ((prompt (format "Type a keystroke to select %s (type ? for help): "
                         what))
         (key (read-key-sequence-vector prompt)))
    (cond ((equal key [7])
           (keyboard-quit))
          ((equal key [??])
           (picp-key-help what)
           (with-current-buffer picp-buffer
             (picp-read-key what)))
          (t (picp-lookup-key-strict key)))))

(defun picp-read-key-to-add-or-remove-tag (&optional remove all)
  (let* ((prompt
          (format "Type a keystroke to select tag to %s %s(type ? for help): "
                  (if remove "remove" "add")
                  (if all "to all pictures " "")))
         (key (read-key-sequence-vector prompt)))
    (cond ((equal key [7])
           (keyboard-quit))
          ((equal key [??])
           (picp-key-help (if remove
                              "tag to remove"
                            "tag to add"))
           (with-current-buffer picp-buffer
             (picp-read-key-to-add-or-remove-tag remove all)))
          ((equal key [?-])
           (picp-read-key-to-add-or-remove-tag (not remove) all))
          (t (list remove (picp-lookup-key-strict key))))))

(defun picp-lookup-key-strict (key)
  (or (picp-lookup-key key)
      (error "Keystroke %s is not defined in picp-keystroke-alist" key)))

(defun picp-lookup-key (x)
  (cl-loop for (key ignored arg) in (picp-keystroke-alist)
           when (equal (picp-key-vector x)
                       (picp-key-vector key))
           return arg))

(defun picp-keystroke-alist ()
  (if (symbolp picp-keystroke-alist)
      (symbol-value picp-keystroke-alist)
    picp-keystroke-alist))


(defun picp-key-vector (key)
  (if (vectorp key)
      key
    (if (stringp key)
        (apply #'vector (listify-key-sequence (kbd key)))
      (vector key))))


(defun picp-describe-keymap (prefix map)
  (map-keymap (lambda (key binding)
                (if (keymapp binding)
                    (picp-describe-keymap (vconcat prefix (vector key)) binding)
                  (unless (eq binding 'undefined)
                    (princ (format "%16s - %s\n"
                                   (key-description (vconcat prefix
                                                             (vector key)))
                                   binding)))))
              map))

(defun picp-keymap-to-list (prefix map predicate)
  (let (list)
    (map-keymap (lambda (key binding)
                  (if (keymapp binding)
                      (setq list
                            (append list
                                    (picp-keymap-to-list (vconcat prefix
                                                                  (vector key))
                                                         binding
                                                         predicate)))
                    (when (funcall predicate binding)
                      (push (cons (vconcat prefix (vector key))
                                  binding)
                            list))))
                map)
    list))

(defun picp-key-sort-string (keystroke)
  (cl-loop for key across keystroke
           concat (format "%10s%2s"
                          (event-basic-type key)
                          (picp-modifier-weigth key))))

(defun picp-modifier-weigth (key)
  (cl-loop for modifier in (event-modifiers key)
           sum (cl-case modifier
                 (shift 1)
                 (control 2)
                 (meta 4)
                 (super 8)
                 (t 0))))



(defun picp-update-keymap ()
  (picp-cleanup-keymap nil picp-mode-map)
  ;; no, this may override user settings....
  ;; (picp-define-keymap picp-mode-map)
  (cl-loop for (key action arg) in (picp-keystroke-alist)
           do (define-key picp-mode-map
                (picp-key-vector key)
                (cond ((memq action '(tag add-tag))
                       (intern arg picp-tag-completion-table)
                       (picp-user-tag-command arg))
                      ((memq action '(move copy hardlink))
                       (picp-user-file-command action arg))
                      ((symbolp action)
                       action)
                      (t
                       (error (concat "Invalid entry in"
                                      " picp-keystroke-alist"
                                      " (%s %s %s)")
                              key action arg)))))
  (when (buffer-live-p (get-buffer picp-buffer))
    (with-current-buffer picp-buffer
      (use-local-map picp-mode-map)))
  (setq picp-old-keystroke-alist (picp-keystroke-alist)))


(defvar picp-tmp-map nil)

(defun picp-cleanup-keymap (key value)
  (cond ((keymapp value)
         (let ((picp-tmp-map value))
           (map-keymap #'picp-cleanup-keymap value)))
        ((and (symbolp value)
              (get value 'picp-user-command)
              (characterp key))
         (define-key picp-tmp-map (vector key) #'undefined))))


(defun picp-user-tag-command (tag)
  "Create a command that add TAG to current picture."
  (let ((symbol (intern (picp-command-name 'add-tag tag))))
    (fset symbol `(lambda ()
                    ,(format "Add tag %s." tag)
                    (interactive)
                    (picp-command
                      (picp-action 'add-tag ,tag)
                      (picp-old-update-buffer))))
    (put symbol 'picp-user-command 'add-tag)
    symbol))

(defun picp-user-file-command (action dst)
  "Create a command that move/copy/hardlink the current picture.
ACTION is one of the symbols move, copy or hardlink.
DST is the destination directory."
  (let ((symbol (intern (picp-command-name action dst))))
    (fset symbol `(lambda ()
                    ,(picp-command-doc action dst)
                    (interactive)
                    (picp-command
                      (picp-action ',action ,dst)
                      ,(when (eq action 'move)
                         '(picp-old-update-buffer)))))
    (put symbol 'picp-user-command 'file)
    symbol))

(defun picp-command-name (action arg)
  (pcase action
    ((or `add-tag `tag) (concat "picp-add-tag-" arg))
    (`move (concat "picp-move-to-" arg))
    (`copy (concat "picp-copy-to-" arg))
    (`hardlink (concat "picp-hardlink-to-" arg))
    (f (symbol-name f))))

(defun picp-command-doc (action dst)
  (format "%s image file to directory %s."
          (capitalize (symbol-name action))
          dst))

;;; Help functions

(defvar picp-help-count 0)
(defvar picp-is-sole-window nil)

(defvar picp-help-map
  (let ((map (make-sparse-keymap)))
    (define-key map [??] 'picp-help)
    map))

(defun picp-help ()
  "Toggle display of help for commands.

First invocation will display help for user defined commands if
there are any.  User defined commands are defined by setting the
variable `picp-keystroke-alist').

Second invocation will display help for built-in commands for
picpocket mode.

Third invocation will hide the help buffer."
  (interactive)
  (picp-command
    (if (eq last-command this-command)
        (setq picp-help-count (1+ picp-help-count))
      (setq picp-help-count 0
            picp-is-sole-window (eq 1 (count-windows))))
    (if (picp-keystroke-alist)
        (pcase picp-help-count
          (0 (picp-help-user-commands)
             (picp-help-finish))
          (1 (picp-help-mode-commands)
             (picp-help-finish))
          (_ (setq picp-help-count -1)
             (picp-hide-help)))
      (pcase picp-help-count
        (0 (picp-help-mode-commands)
           (picp-help-finish))
        (_ (setq picp-help-count -1)
           (picp-hide-help))))))

(defun picp-help-finish ()
  (when picp-is-sole-window
    (with-selected-window (picp-visible-window (help-buffer))
      (picp-shrink-to-fit)))
  ;; This is only needed if help buffer was selected,
  ;; see `help-window-select'.
  (set-transient-map picp-help-map))

(defun picp-hide-help ()
  (let ((help (picp-visible-window (help-buffer))))
    (when help
      (if picp-is-sole-window
          (delete-window help)
        (with-selected-window help
          (quit-window))))))

(defun picp-visible-window (buffer-name)
  (cl-loop for window being the windows
           when (string-equal buffer-name
                              (buffer-name (window-buffer window)))
           return window))

(defun picp-visible-buffers ()
  (mapcar (lambda (window)
            (buffer-name (window-buffer window)))
          (window-list)))

(defun picp-shrink-to-fit ()
  (when (window-combined-p nil t)
    (shrink-window-horizontally (- (window-width) (picp-buffer-width) 1))))

(defun picp-buffer-width ()
  (save-excursion
    (goto-char (point-min))
    (cl-loop until (eobp)
             maximize (- (point-at-eol) (point-at-bol))
             do (forward-line 1))))


(defun picp-help-mode-commands ()
  (help-setup-xref (list #'picp-help-mode-commands)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (princ "Picpocket mode commands:\n\n")
      (princ "key             binding\n")
      (princ "---             -------\n\n")
      (let* ((predicate (lambda (binding)
                          (not (or (eq binding 'undefined)
                                   (get binding 'picp-user-command)))))
             (commands (sort (picp-keymap-to-list nil
                                                  picp-mode-map
                                                  predicate)
                             (lambda (a b)
                               (string-lessp
                                (picp-key-sort-string (car a))
                                (picp-key-sort-string (car b)))))))
        (cl-loop for (key . binding) in commands
                 do (progn
                      (princ (format "%-16s" (key-description key)))
                      (if (eq binding 'picp-repeat)
                          (picp-repeat-help)
                        (princ (symbol-name binding)))
                      (princ "\n")))))))

(defun picp-repeat-help ()
  ;; Help mode will make hyperlinks for all commands found at end of
  ;; line.  For picp-repeat we add stuff after command name so that
  ;; will not trigger.  Therefore make our own hyperlink for
  ;; picp-repeat.
  (insert-text-button "picp-repeat"
                      'type 'help-function
                      'help-args (list 'picp-repeat))
  (princ (format " (%s %s)" picp-last-action picp-last-arg)))


(defun picp-help-user-commands ()
  (help-setup-xref (list #'picp-help-user-commands)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (princ "User defined picpocket commands:\n\n")
    (princ "key             binding\n")
    (princ "---             -------\n\n")
    (cl-loop for (key action arg) in (picp-keystroke-alist)
             do (princ (format "%-16s%s\n"
                               (key-description (picp-key-vector key))
                               (picp-command-name action arg))))))

(defun picp-key-help (&optional what)
  (help-setup-xref (list #'picp-key-help what)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (setq what (or what "directory/tag"))
    (princ (format "key             %s\n" what))
    (princ (format "---             %s\n\n" (make-string (length what) ?-)))
    (when (string-equal what "tag to remove")
      (princ (format "-               add tag instead of remove\n")))
    (when (string-equal what "tag to add")
      (princ (format "-               remove tag instead of add\n")))
    (cl-loop for (key ignored arg) in (picp-keystroke-alist)
             do (princ (format "%-16s%s\n"
                               (key-description (picp-key-vector key))
                               arg)))))


;;; Undoable actions functions

(defvar picp-undo-ewoc nil)
(defvar picp-undo-legend-ewoc nil)
(defvar picp-undo-window nil)
(defvar picp-current-undo-node nil)
(defvar picp-trashcan nil)

(defface picp-dim-face '((t (:foreground "gray")))
  "Face for unavailable commands.")



(cl-defstruct picp-undoable
  ;; State is the symbol incomplete, done or undone.
  state
  ;; Action is a symbol in the list picp-undoable-actions.
  action
  ;; Arg is a string dependant on action:
  ;;   add-tag    - tag
  ;;   remove-tag - tag
  ;;   set-tags   - space-separated tags
  ;;   delete     - nil
  ;;   rename     - new absolute filename (directory may be changed)
  ;;   move       - new absolute directory
  ;;   copy       - absolute destination directory
  ;;   hardlink   - absolute destination directory
  arg
  ;; If all is non-nil then the action is applied to all pictures in
  ;; the current picpocket list.
  all
  ;; ops is a list of picp-op structs.  Usually there is only a single
  ;; operation in this list.  But in case the all slot is non-nil
  ;; there will be one entry in this list per picture.
  ops)

(cl-defstruct picp-op
  action
  file
  sha
  to-file
  trash-file
  tags
  tag)

(cl-defstruct picp-legend
  key
  text
  predicate)

(defconst picp-undoable-actions '(add-tag remove-tag set-tags delete
                                 rename move copy hardlink))
(defconst picp-repeatable-actions '(move copy hardlink add-tag remove-tag))

(unless (cl-subsetp picp-repeatable-actions picp-undoable-actions)
  (error "Some repeatable action is not undoable"))

(defun picp-action (action arg &optional pic)
  "All undoable actions go through this function.

A subset of the picpocket commands trigger undoable actions.
These are listed in the constant `picp-undoable-actions'.  ACTION
is a symbol from this list.

A subset of the undoable actions are repeatable.  These are
listed in the constant `picp-repeatable-actions'.  Repeatable
actions can be repeated with the command `picp-repeat'.

ARG is a string.  For file operations it is the destination
directory or filename.  For tag operations it is the tag or tags
separated with space.

PIC is the picture to work on.  It defaults to `picp-current'.
If PIC is the symbol `all' then the action is applied to all
pictures in the current picpocket list (this is not supported for
the delete action, though)."
  (unless (memq action picp-undoable-actions)
    (error "Action %s is not undoable" action))
  (picp-stash-undo-begin :action action
                         :arg arg
                         :all (eq pic 'all))
  (if (eq pic 'all)
      (let ((pic picp-list)
            next)
        (while pic
          (setq next (cdr pic))
          (when (picp-filter-match-p pic)
            (picp-do-action action arg pic))
          (setq pic next)))
    (picp-ensure-current-pic)
    (picp-do-action action arg pic)
    (when (memq action picp-repeatable-actions)
      (picp-save-repeatable-action action arg)))
  (picp-stash-undo-end))

(defun picp-save-repeatable-action (action arg)
  (setq picp-last-action action
        picp-last-arg arg))

;; Currently single pic actions print message here.
;; PENDING - Move the message calls to the sub-routines....
;; Callers of picp-action with 'all also print a summary
;; message when all pictures are handled.
(defun picp-do-action (action arg pic)
  (pcase action
    (`set-tags
     (picp-set-tags-action arg pic)
     (if (picp-tags pic)
         (message "Tags set to %s." (picp-format-tags (picp-tags pic)))
       (message "Tags cleared")))
    (`add-tag
     (picp-add-tag-action arg pic)
     (message "%s is tagged with %s." (picp-file pic) arg))
    (`remove-tag
     (picp-remove-tag-action arg pic)
     (message "Tag %s is removed from %s." arg (picp-file pic)))
    (`delete
     (when (eq 'all pic)
       (error "Refusing to delete all pictures"))
     (let ((file (picp-file pic)))
       (picp-delete-action pic)
       (message "%s is no more." file)))
    ((or `move `rename `copy `hardlink)
     (picp-file-action action arg pic))
    (_
     (error "Unknown action %s %s" action arg))))

(defvar picp-undo-fail nil)
(defvar picp-undo-ok nil)

(defun picp-undo-action (undoable)
  (unless (picp-undoable-is-undoable-p undoable)
    (error "Action is not undoable"))
  (let (picp-undo-fail picp-undo-ok)
    (dolist (op (picp-undoable-ops undoable))
      (picp-undo-op op))
    (cond ((and (null picp-undo-fail) (null picp-undo-ok))
           (message "Nothing to undo")
           (setf (picp-undoable-state undoable) 'undone))
          ((null picp-undo-fail)
           (message "Undo ok %s"
                    (picp-undo-summary picp-undo-ok))
           (setf (picp-undoable-state undoable) 'undone))
          ((null picp-undo-ok)
           (message "Undo failed %s"
                    (picp-undo-summary picp-undo-fail)))
          (t
           (message "Undo partly failed (%s actions failed, %s actions ok)"
                    (length picp-undo-fail)
                    (length picp-undo-ok))
           (setf (picp-undoable-state undoable) 'incomplete)))))

(defun picp-undo-op (op)
  (pcase (picp-op-action op)
    (`delete (picp-undo-delete-action op))
    (`add-tag (picp-undo-add-tag-action op))
    (`remove-tag (picp-undo-remove-tag-action op))
    ((or `rename `move) (picp-undo-file-relocate-action op))
    ((or `copy `hardlink) (picp-undo-file-duplicate-action op))))

(defun picp-undo-summary (list)
  (if (cdr list)
      (format "(%s actions)" (length list))
    (format "(%s)" (car list))))

(defun picp-undo-fail (format &rest args)
  (let ((text (apply #'format format args)))
    (message text)
    (warn text)
    (push text picp-undo-fail)))

(defun picp-undo-ok (format &rest args)
  (let ((text (apply #'format format args)))
    (message text)
    (push text picp-undo-ok)))



(defun picp-delete-action (pic)
    (let ((inhibit-quit t)
          (file (picp-absfile pic))
          (filter-match (picp-filter-match-p pic))
          (trash-file (picp-trash-file (picp-file pic))))
      (picp-stash-undo-op :action 'delete
                          :file (picp-absfile pic)
                          :tags (picp-tags pic)
                          :trash-file trash-file)
      (rename-file file trash-file)
      (picp-tags-delete-file pic file)
      (picp-list-delete pic (list filter-match))))


(defun picp-undo-delete-action (op)
  (let ((trash-file (picp-op-trash-file op))
        (file (picp-op-file op))
        (tags (picp-op-tags op))
        (inhibit-quit t))
    (if (not (file-exists-p trash-file))
        (picp-undo-fail "Cannot undelete %s, %s does not exist"
                        (file-name-nondirectory file)
                        trash-file)
      (make-directory (file-name-directory file) t)
      (rename-file trash-file file)
      (picp-list-insert-before-current (picp-make-pic file))
      (picp-tags-set picp-current tags)
      (picp-undo-ok "Undeleted %s" (file-name-nondirectory file)))))

(defun picp-trash-file (filename)
  (setq filename (file-name-nondirectory filename))
  (unless picp-trashcan
    (setq picp-trashcan (file-name-as-directory
                         (make-temp-file "picpocket-trash" t))))
  (make-directory picp-trashcan t)
  (cl-loop for i = 1 then (1+ i)
           for f = (picp-trash-file-candidate filename i)
           unless (file-exists-p f)
           return f))

(defun picp-trash-file-candidate (filename i)
  (if (eq i 1)
      (expand-file-name filename picp-trashcan)
    (concat picp-trashcan
            (file-name-sans-extension filename)
            "_"
            (number-to-string i)
            (if (file-name-extension filename) "." "")
            (file-name-extension filename))))


(defun picp-add-tag-action (tag-string &optional pic)
  (let* ((pic (or pic picp-current))
         (tag (intern tag-string))
         (tags (picp-tags pic))
         (inhibit-quit t))
    (unless (memq tag tags)
      (picp-tags-set pic (append tags (list tag)))
      (picp-stash-undo-op :action 'add-tag
                          :file (picp-absfile pic)
                          :sha (picp-sha-force pic)
                          :tag tag))))


(defun picp-undo-add-tag-action (op)
  (let* ((file (picp-op-file op))
         (tag (picp-op-tag op))
         (sha (picp-op-sha op))
         (current-tags (picp-db-tags sha)))
    (when (memq tag current-tags)
      (picp-db-tags-set sha file (delq tag current-tags))
      (picp-reset-filter-counters))
    (picp-undo-ok "Undo add tag %s to %s"
                  tag
                  (file-name-nondirectory file))))

(defun picp-remove-tag-action (tag-string &optional pic)
  (let* ((pic (or pic picp-current))
         (tag (intern tag-string))
         (tags (picp-tags pic))
         (inhibit-quit t))
    (when (memq tag tags)
      (picp-tags-set pic (delq tag tags))
      (picp-stash-undo-op :action 'remove-tag
                          :file (picp-absfile pic)
                          :sha (picp-sha-force pic)
                          :tag tag))))

(defun picp-undo-remove-tag-action (op)
  (let* ((file (picp-op-file op))
         (tag (picp-op-tag op))
         (sha (picp-op-sha op))
         (current-tags (picp-db-tags sha)))
    (unless (memq tag current-tags)
      (picp-db-tags-set sha file (append current-tags (list tag)))
      (picp-reset-filter-counters))
    (picp-undo-ok "Undo remove tag %s from %s"
                  tag
                  (file-name-nondirectory file))))

(defun picp-set-tags-action (tags-string pic)
  (let* ((pic (or pic picp-current))
         (old-tags (picp-tags pic))
         (new-tags (picp-tags-string-to-list tags-string))
         (inhibit-quit t))
    (picp-tags-set pic new-tags)
    (dolist (tag (cl-set-difference old-tags new-tags))
      (picp-stash-undo-op :action 'remove-tag
                          :file (picp-absfile pic)
                          :sha (picp-sha-force pic)
                          :tag tag))
    (dolist (tag (cl-set-difference new-tags old-tags))
      (picp-stash-undo-op :action 'add-tag
                          :file (picp-absfile pic)
                          :sha (picp-sha-force pic)
                          :tag tag))))


(defun picp-tags-string-to-list (tags-string)
  (cl-delete-duplicates
   (mapcar #'intern
           (split-string tags-string))))

(defun picp-new-path-for-file-action (action dst pic)
  (file-truename
   (if (eq action 'rename)
       dst
     (expand-file-name (picp-file pic)
                       (if (or (file-name-absolute-p dst)
                               picp-destination-relative-current)
                           dst
                         (expand-file-name dst picp-destination-dir))))))

(defun picp-file-action (action dst pic)
  (let* ((pic (or pic picp-current))
         (new-path (picp-new-path-for-file-action action dst pic))
         (old-dir (picp-dir pic))
         (old-file (picp-file pic))
         (old-path (concat old-dir old-file))
         (new-dir (file-name-directory new-path))
         (new-file (file-name-nondirectory new-path))
         (ok-if-already-exists noninteractive))
    (make-directory new-dir t)
    (while (and (file-exists-p new-path)
                (not ok-if-already-exists))
      (cond ((equal old-path new-path)
             (user-error "Attempt to %s file to itself"
                         (symbol-name action)))
            ((file-directory-p new-path)
             (error "%s already exists as a directory" new-path))
            ((picp-files-identical-p old-path new-path)
             (if (y-or-n-p (concat "Identical file already exists in "
                                   new-dir ".  Overwrite? "))
                 (setq ok-if-already-exists t)
               (user-error "Not overwriting %s" new-path)))
            (t
             (setq new-file (picp-compare pic new-path)
                   new-path (concat new-dir new-file)))))
    (pcase action
      ((or `move `rename)
       (picp-file-relocate-action action old-path new-path pic))
      ((or `copy `hardlink)
       (picp-file-duplicate-action action old-path new-path pic))
      (_ (error "Invalid picpocket action %s" action)))))

(defun picp-files-identical-p (a b)
  (and (file-exists-p a)
       (file-exists-p b)
       (let ((a-bytes (picp-file-bytes a))
             (b-bytes (picp-file-bytes b)))
         (eq a-bytes b-bytes))
       (if (executable-find "diff")
           (zerop (call-process "diff" nil nil nil "-q"
                                (expand-file-name a)
                                (expand-file-name b)))
         (picp-elisp-files-identical-p a b))))

(defun picp-elisp-files-identical-p (a b)
  (string-equal (picp-file-content a)
                (picp-file-content b)))

(defun picp-file-content (file)
  (with-temp-buffer
    (buffer-disable-undo)
    (insert-file-contents-literally file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun picp-file-relocate-action (action old-path new-path pic)
  (let ((new-dir (file-name-directory new-path))
        (new-file (file-name-nondirectory new-path))
        (old-dir (file-name-directory old-path))
        (old-file (file-name-nondirectory old-path))
        (inhibit-quit t)
        trash-file)
    (when (file-exists-p new-path)
      (setq trash-file (picp-trash-file new-file))
      (rename-file new-path trash-file))
    (picp-stash-undo-op :action action
                        :file old-path
                        :to-file new-path
                        :trash-file trash-file
                        :sha (picp-sha-force pic))
    (rename-file old-path new-path t)
    (picp-tags-move-file pic old-path new-path)
    (cond ((or (eq action 'move)
               (equal old-file new-file))
           (picp-list-delete pic)
           (message "Moved %s to %s."
                    (file-name-nondirectory old-path)
                    (file-name-directory new-path)))
          ((equal old-dir new-dir)
           (picp-set-file pic new-file)
           (message "Renamed %s to %s." old-file new-file))
          (t
           (picp-list-delete pic)
           (message "Renamed and moved %s to %s." old-file new-path)))))

(defun picp-undo-file-relocate-action (op)
  (let ((file (picp-op-file op))
        (to-file (picp-op-to-file op))
        (trash-file (picp-op-trash-file op))
        (sha (picp-op-sha op))
        (inhibit-quit t))
    (cond ((not (file-exists-p to-file))
           (picp-undo-fail "Cannot undo %s, %s does not exist"
                           (picp-op-action op)
                           to-file))
          ((file-exists-p file)
           (picp-undo-fail "Cannot undo %s, %s already exist"
                           (picp-op-action op)
                           file))
          (t
           (make-directory (file-name-directory file) t)
           (rename-file to-file file)
           (picp-db-tags-move-file sha to-file file)
           (when trash-file
             (rename-file trash-file to-file))
           (let ((pic (picp-list-search to-file)))
             (if pic
                 (picp-set-absfile pic file)
               (picp-list-insert-before-current (picp-make-pic file))))
           (picp-undo-ok "%s %s back"
                         (picp-action-past-tense (picp-op-action op))
                         (file-name-nondirectory file))))))

(defun picp-file-duplicate-action (action old-path new-path pic)
  (let ((old-file (file-name-nondirectory old-path))
        (inhibit-quit t)
        trash-file)
    (picp-tags-copy-file picp-current new-path)
    (when (file-exists-p new-path)
      (setq trash-file (picp-trash-file new-path))
      (rename-file new-path trash-file))
    (if (eq action 'copy)
        (copy-file old-path new-path t)
      (add-name-to-file old-path new-path t))
    (picp-stash-undo-op :action action
                        :file old-path
                        :to-file new-path
                        :trash-file trash-file
                        :sha (picp-sha-force pic))
    (picp-duplicate-message action old-file new-path)))

(defun picp-undo-file-duplicate-action (op)
  (let ((to-file (picp-op-to-file op))
        (trash-file (picp-op-trash-file op))
        (sha (picp-op-sha op))
        (inhibit-quit t))
    (cond ((not (file-exists-p to-file))
           (picp-undo-fail "Cannot undo %s, %s does not exist"
                           (picp-op-action op)
                           to-file))
          (t
           (delete-file to-file)
           (when trash-file
             (rename-file trash-file to-file))
           (picp-db-tags-delete-file sha to-file)
           (if (eq 'copy (picp-op-action op))
               (picp-undo-ok "Uncopied %s"
                             (file-name-nondirectory to-file))
             (picp-undo-ok "Un-hard-linked %s"
                           (file-name-nondirectory to-file)))))))

(defun picp-duplicate-message (action old dst)
  (message "%s %s to %s."
           (if (eq action 'copy)
               "Copied"
             "Hard linked")
           old
           dst))

(defun picp-compare (pic new-path)
  (unwind-protect
      (let (picp-adapt-to-window-size-change)
        (picp-show-two-pictures pic new-path)
        (read-string (format (concat "File already exists (size %s)."
                                     "  Rename this (size %s) to: ")
                             (picp-kb (picp-file-bytes new-path))
                             (picp-kb (picp-bytes-force pic)))
                     (picp-file pic)))
    (picp-update-buffer)))

(defun picp-show-two-pictures (pic new)
  (picp-ensure-picpocket-buffer)
  (cl-destructuring-bind (window-width . window-height)
      (picp-save-window-size)
    (let* ((line-height (+ (frame-char-height)
                           (or line-spacing
                               (frame-parameter nil 'line-spacing)
                               0)))
           (pic-height (/ (- window-height (* 2 line-height)) 2))
           (picp-fit (picp-standard-value 'picp-fit))
           (picp-scale (picp-standard-value 'picp-scale))
           buffer-read-only)
      (erase-buffer)
      (insert (format "About to overwrite this picture (%s):\n"
                      (picp-kb (picp-file-bytes new))))
      (insert-image (picp-create-image (list (picp-make-pic new))
                                       (cons window-width pic-height)))
      (insert (format "\nWith this picture (%s):\n"
                      (picp-kb (picp-bytes-force pic))))
      (insert-image (picp-create-image pic (cons window-width pic-height)))
      (goto-char (point-min)))))

(defun picp-standard-value (symbol)
  (eval (car (get symbol 'standard-value))))


;;; Undo commands

(defun picp-undo ()
  "Undo last command.
\\<picp-mode-map>
Most commands are undoable.  All commands that delete, move, copy
or hardlink files are undoable.  Also all commands that adds or
removes tags.  This includes commands that have been defined by
the user customizing variable `picp-keystroke-alist'.

Navigational commands and commands that affect the
display (rotation and scaling) are not undoable.

The last `picp-undo-list-size' undoable commands are saved.  Type
\\[picp-visit-undo-list] to view a list of them in a special
buffer.  In that buffer it is possible to select and undo any
command in the list.

This command picks the first undoable command in that list."
  (interactive)
  (picp-command
    (let ((undoable (when picp-undo-ring
                    (cl-loop for i from 0 to (ring-length picp-undo-ring)
                             for undoable = (ring-ref picp-undo-ring i)
                             while undoable
                             when (picp-undoable-is-undoable-p undoable)
                             return undoable))))
      (if undoable
          (picp-undo-action undoable)
        (user-error "No undoable actions have been done")))))


(defun picp-undoable-is-undoable-p (undoable)
  (memq (picp-undoable-state undoable)
        '(done incomplete)))

(defun picp-visit-undo-list ()
  "List the current undoable commands in a separate buffer."
  (interactive)
  (picp-bye-command
    (when (> (window-width) 100)
      (setq picp-undo-window (split-window-right)))
    (switch-to-buffer (get-buffer-create picp-undo-buffer))
    (with-current-buffer picp-undo-buffer
      (picp-undo-mode)
      (picp-update-undo-buffer))))


;;; Undo stash functions

(defun picp-stash-undo-begin (&rest args)
  (unless picp-undo-ring
    (setq picp-undo-ring (make-ring picp-undo-list-size)))
  (picp-maybe-grow-undo-ring)
  (picp-remove-empty-incomplete-entries)
  (picp-remove-one-if-stash-is-full)
  (ring-insert picp-undo-ring
               (apply #'make-picp-undoable
                      :state 'incomplete
                      args)))

(defun picp-maybe-grow-undo-ring ()
  ;; Grow if needed, but in contrast the ring is never shrinked
  ;; dynamically.
  (let ((size (ring-size picp-undo-ring)))
    (when (> picp-undo-list-size size)
      (ring-extend picp-undo-ring (- picp-undo-list-size size)))))

(defun picp-remove-empty-incomplete-entries ()
  (while (let ((newest (and (not (ring-empty-p picp-undo-ring))
                            (ring-ref picp-undo-ring 0))))
           (and newest
                (eq (picp-undoable-state newest) 'incomplete)
                (null (picp-undoable-ops newest))))
    (picp-cleanup-undo-entry (ring-remove picp-undo-ring 0))))

(defun picp-remove-one-if-stash-is-full ()
  (when (= (ring-length picp-undo-ring)
           (ring-size picp-undo-ring))
    (picp-cleanup-undo-entry (ring-remove picp-undo-ring))))

(defun picp-cleanup-undo-entry (undo)
  (dolist (op (picp-undoable-ops undo))
    (and (picp-op-trash-file op)
         (file-exists-p (picp-op-trash-file op))
         (delete-file (picp-op-trash-file op)))))

(defun picp-stash-undo-op (&rest args)
  (when (or (null picp-undo-ring)
            (ring-empty-p picp-undo-ring))
    (error "Call to picp-stash-undo-op before picp-stash-undo-begin"))
  ;; PENDING - maybe should append instead of push?
  ;; Currently picp-stash-undo-end is reversing the list.
  ;; (let ((undoable (ring-ref picp-undo-ring 0)))
  ;; (setf (picp-undoable-ops undoable)
  ;; (append (picp-undoable-ops undoable)
  ;; (list (apply #'make-picp-op args))))))
  (push (apply #'make-picp-op args)
        (picp-undoable-ops (ring-ref picp-undo-ring 0))))

(defun picp-stash-undo-end ()
  (let ((current-undo (ring-ref picp-undo-ring 0)))
    (setf (picp-undoable-ops current-undo)
          (reverse (picp-undoable-ops current-undo)))
    (setf (picp-undoable-state current-undo) 'done)))




;;; The undo buffer's ewoc population

;; There are two ewocs in the undo buffer.
;;
;; One is called picp-legend-ewoc and shows a legend for the available
;; commands in the undo-buffer.  The node data is instances of the
;; struct picp-undo-legend.  The text for a command is dimmed out if
;; it is not appropriate for the thing at point.
;;
;; The other is called picp-undo-ewoc and shows the list of undoable
;; things.  The node data is instances of the struct picp-undoable.  It is
;; a mirror of the picp-undo-ring - it contain the same data in the
;; same order.  Whenever a command alter the picp-undo-ring the
;; picp-undo-ewoc will be rebuilt from scratch (the picp-command macro
;; takes care of that).



(defun picp-update-undo-buffer ()
  (with-current-buffer picp-undo-buffer
    (let ((progress nil)
          (i 0)
          (start-time (current-time))
          (buffer-read-only nil))
      (erase-buffer)
      (insert "\n"
              (picp-emph "Picpocket undo buffer")
              "\n")
      (setq picp-undo-legend-ewoc (ewoc-create #'picp-undo-legend-pp))
      (picp-undo-legend-add "u"
                            "undo an entry"
                            #'picp-current-undoable-p)
      ;; (picp-undo-legend-add "r"
      ;; "redo an entry"
      ;; #'picp-current-redoable-p)
      (picp-undo-legend-add "n"
                            "move to next entry"
                            #'picp-current-have-next-p)
      (picp-undo-legend-add "p"
                            "move to previous entry"
                            #'picp-current-have-previous-p)
      (picp-undo-legend-add "q"
                            "return to picpocket buffer"
                            #'picp-true)
      (goto-char (point-max))
      (insert "List of undoable actions with the most recent first:\n")
      (setq picp-current-undo-node nil
            picp-undo-ewoc (ewoc-create #'picp-undo-pp
                                        nil nil t))
      (if (or (null picp-undo-ring)
              (ring-empty-p picp-undo-ring))
          (insert "\n(There is nothing to undo)")
        (dolist (undoable (ring-elements picp-undo-ring))
          (ewoc-enter-last picp-undo-ewoc undoable)
          (cl-incf i)
          (when (picp-more-than-half-a-second-since-p start-time)
            (setq progress (or progress (make-progress-reporter
                                         "Making undo buffer "
                                         0
                                         (ring-length picp-undo-ring))))
            (progress-reporter-update progress i))))
      (picp-init-current-undo)
      (picp-update-current-undo)
      (when progress
        (progress-reporter-done progress)))))

(defun picp-more-than-half-a-second-since-p (time)
  (time-less-p (seconds-to-time 0.5)
               (time-subtract (current-time) time)))


(defun picp-undo-pp (undoable)
  (insert (if (and picp-current-undo-node
                   (eq undoable (ewoc-data picp-current-undo-node)))
              (picp-emph " -> ")
            "    ")
          (capitalize (symbol-name (picp-undoable-state undoable)))
          " action: "
          (picp-undoable-text undoable))
  (insert "\n      ")
  (let ((ops (picp-undoable-ops undoable)))
    (cl-loop for i from 0 to (1- picp-max-undo-thumbnails)
             for op = (elt ops i)
             while op
             do (picp-mini-image undoable op)
             do (insert " "))
    (when (elt ops picp-max-undo-thumbnails)
      (insert "....")))
  (insert "\n")
  (insert (propertize "\n" 'line-height 1.5)))

(defun picp-mini-image (undoable &optional op)
  (let ((op (or op (car (picp-undoable-ops undoable)))))
    (picp-insert-mini-image (picp-op-image-file undoable op))))

(defun picp-insert-mini-image (file)
  (and (display-images-p)
       file
       (file-exists-p file)
       (insert-image (create-image file
                                   (picp-image-type file)
                                   nil
                                   :height (* picp-undo-thumbnails-size
                                              (frame-char-height))))))

(defun picp-op-image-file (undoable op)
  (if (eq (picp-undoable-state undoable) 'undone)
      (picp-op-file op)
    (pcase (picp-undoable-action undoable)
      ((or `set-tags `add-tag `remove-tag) (picp-op-file op))
      (`delete (picp-op-trash-file op))
      (_ (picp-op-to-file op)))))


(defconst picp-empty-op (make-picp-op :file "nothing"))

(defun picp-undoable-text (undoable)
  (let* ((action (picp-undoable-action undoable))
         (arg (picp-undoable-arg undoable))
         (ops (picp-undoable-ops undoable))
         (first-op (or (car ops) picp-empty-op))
         (file (if (picp-undoable-all undoable)
                   (format "all %s pictures" (length ops))
                 (file-name-nondirectory (picp-op-file first-op)))))
    (pcase action
      (`set-tags (concat "set tags to "
                         (picp-format-tags arg)
                         " on "
                         file))
      (`add-tag (concat "add tag "
                        arg
                        " to "
                        file))
      (`remove-tag (concat "remove tag "
                           arg
                           " from "
                           file))
      (`delete (concat "delete " file))
      (_ (concat (symbol-name action)
                 " "
                 file
                 " to "
                 arg)))))


(defun picp-undo-legend-add (key text predicate)
  (ewoc-enter-last picp-undo-legend-ewoc
                   (make-picp-legend :key key
                                     :text text
                                     :predicate predicate)))

(defun picp-current-have-previous-p (current)
  (and current
       (not (eq current
                (picp-first-undo-node)))))

(defun picp-current-have-next-p (current)
  (and current
       (not (eq current
                (picp-last-undo-node)))))

(defun picp-current-undoable-p (current)
  (and current
       (memq (picp-undoable-state (ewoc-data current))
             '(done incomplete))))

(defun picp-current-redoable-p (current)
  (and current
       (eq (picp-undoable-state (ewoc-data current))
           'undone)))

(defun picp-true (&rest ignored)
  t)

(defun picp-undo-legend-pp (legend)
  (let ((valid (funcall (picp-legend-predicate legend)
                        picp-current-undo-node)))
    (insert (propertize (concat "  Type "
                                (if valid
                                    (picp-emph (picp-legend-key legend))
                                  (picp-legend-key legend))
                                " to "
                                (picp-legend-text legend)
                                ".")
                        'font-lock-face
                        (if valid
                            'default
                          'picp-dim-face)))))


(defun picp-init-current-undo ()
  (setq picp-current-undo-node (picp-first-undo-node)))
;; (when picp-current-undo-node
;; (let ((old-undo (ewoc-data picp-current-undo-node)))
;; (setq picp-current-undo-node
;; (picp-ewoc-find-node picp-undo-ewoc old-undo))))
;; (unless picp-current-undo-node
;; (picp-when-let (first-node (picp-first-undo-node))
;; (setq picp-current-undo-node first-node))))

(defun picp-update-current-undo ()
  (when picp-current-undo-node
    (ewoc-invalidate picp-undo-ewoc picp-current-undo-node)
    (ewoc-goto-node picp-undo-ewoc picp-current-undo-node))
  (save-excursion
    (ewoc-refresh picp-undo-legend-ewoc)))

(defun picp-first-undo-node ()
  (ewoc-nth picp-undo-ewoc 0))

(defun picp-last-undo-node ()
  (ewoc-nth picp-undo-ewoc -1))

(defun picp-ewoc-find-node (ewoc data)
  (cl-loop for i = 0 then (1+ i)
           for node = (ewoc-nth ewoc i)
           while node
           when (eq data (ewoc-data node))
           return node))


;;; The undo buffer's commands

(define-derived-mode picp-undo-mode picp-base-mode "picpocket-undo"
  "Major mode for picpocket undo buffer.")

(let ((map (make-sparse-keymap)))
  (suppress-keymap map)
  (define-key map [?u] #'picp-undo-undo)
  ;; (define-key map [?r] #'picp-undo-redo)
  (define-key map [?n] #'picp-undo-next)
  (define-key map [?p] #'picp-undo-previous)
  (define-key map [return] #'picp-select-undo-entry-at-point)
  (define-key map [?q] #'picp-undo-quit)
  (setq picp-undo-mode-map map))

(defun picp-undo-undo ()
  "Undo the current action."
  (interactive)
  (unless picp-current-undo-node
    (picp-select-undo-entry-at-point))
  (unless picp-current-undo-node
    (error "No action available"))
  (picp-undo-action (ewoc-data picp-current-undo-node))
  (picp-update-picp-buffer)
  (ewoc-invalidate picp-undo-ewoc picp-current-undo-node))

;; (defun picp-undo-redo ()
;; (interactive)
;; (unless picp-current-undo-node
;; (picp-select-undo-entry-at-point))
;; (unless picp-current-undo-node
;; (error "No action available"))
;; ...
;; (picp-update-picp-buffer)
;; (ewoc-invalidate picp-undo-ewoc picp-current-undo-node))

(defun picp-select-undo-entry-at-point ()
  "Select the action at point."
  (interactive)
  (picp-undo-move 0))

(defun picp-undo-next ()
  "Move forward to next action."
  (interactive)
  (picp-undo-move 1))

(defun picp-undo-previous ()
  "Move backward to previous action."
  (interactive)
  (picp-undo-move -1))

(defun picp-undo-move (direction)
  (unless (picp-first-undo-node)
    (error "No undoable or redoable actions available"))
  (let ((old-node picp-current-undo-node))
    (setq picp-current-undo-node nil)
    (when old-node
      (ewoc-invalidate picp-undo-ewoc old-node)))
  (pcase direction
    (-1 (ewoc-goto-prev picp-undo-ewoc 1))
    (1 (ewoc-goto-next picp-undo-ewoc 1)))
  (setq picp-current-undo-node (ewoc-locate picp-undo-ewoc))
  (picp-update-current-undo)
  (picp-show-legend-at-top))

(defun picp-show-legend-at-top ()
  (when (eq picp-current-undo-node (picp-first-undo-node))
    (recenter)))

(defun picp-undo-quit ()
  "Delete the undo buffer and go back to the picpocket buffer."
  (interactive)
  (kill-buffer picp-undo-buffer)
  (when (window-live-p picp-undo-window)
    (delete-window picp-undo-window))
  (if (null (buffer-live-p (get-buffer picp-buffer)))
      (message "No picpocket buffer found")
    (picp-update-picp-buffer)
    (or (picp-select-visible-picpocket-window)
        (switch-to-buffer picp-buffer))))

(defun picp-update-picp-buffer ()
  (when (buffer-live-p (get-buffer picp-buffer))
    (with-current-buffer picp-buffer
      (picp-update-buffer))))

(defun picp-select-visible-picpocket-window ()
  "Return non-nil if visible picpocket window was found."
  (cl-loop for window in (window-list nil 'no-mini)
           when (equal (buffer-name (window-buffer window))
                       picp-buffer)
           return (progn
                    (select-window window)
                    t)))




;;; Header line functions

(defvar picp-last-msg nil)
(defvar picp-last-msg-timestamp nil)
(defvar picp-show-msg-seconds 1.5)

(defun picp-update-header-seconds ()
  (+ picp-show-msg-seconds 0.1))

(defun picp-header-line ()
  (if (eq (selected-frame) picp-frame)
      (picp-fullscreen-header-line)
    (picp-header-pic-info)))

(defun picp-fullscreen-header-line ()
  (let ((msg (picp-escape-percent (current-message))))
    (picp-msg "  msg " msg)
    (cond ((null msg)
           (if (or (null picp-last-msg)
                   (picp-last-msg-too-old-p))
               (progn
                 (picp-msg "  null msg - nothing")
                 (setq picp-last-msg nil)
                 (picp-header-pic-info))
             (picp-msg "  null msg - show picp-last-msg")
             picp-last-msg))
          ((null picp-current)
           (setq picp-last-msg nil)
           msg)
          ((not (string-equal msg picp-last-msg))
           (picp-msg "  fresh msg")
           (setq picp-last-msg msg
                 picp-last-msg-timestamp (current-time))
           msg)
          ((not (picp-last-msg-too-old-p))
           (picp-msg "  keep msg")
           msg)
          (t
           (picp-msg "  pic-info")
           (picp-header-pic-info)))))

(defun picp-last-msg-too-old-p ()
  (time-less-p (seconds-to-time picp-show-msg-seconds)
               (time-since picp-last-msg-timestamp)))

(defun picp-update-header (&rest ignored)
  (picp-msg "picp-update-header: called")
  (when (eq (selected-frame) picp-frame)
    (picp-msg "picp-update-header: fullscreen")
    (force-mode-line-update)))

;; PENDING - keeping this debug stuff for now.
(defvar picp-start (current-time))
(defun picp-msg (&rest args)
  (when nil
    (with-current-buffer (get-buffer-create "*piclog*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%f %s\n"
                        (time-to-seconds (time-since picp-start))
                        (mapconcat (lambda (x)
                                     (if (stringp x)
                                         x
                                       (prin1-to-string x)))
                                   args
                                   " ")))))))


(defun picp-header-pic-info ()
  (and picp-current
       picp-list
       picp-db
       (picp-join (concat (format "%s/%s " picp-index picp-list-length)
                          (picp-escape-percent (picp-header-dir))
                          "/"
                          (propertize (picp-escape-percent (picp-file))
                                      'face 'highlight))
                  (when picp-debug
                    picp-header-text)
                  (picp-maybe-kb)
                  (picp-scale-info)
                  (picp-rotation-info)
                  (picp-format-tags (picp-tags picp-current))
                  (picp-filter-info))))

(defun picp-join (&rest strings)
  (mapconcat 'identity
             (delete nil (delete "" strings))
             " "))

(defun picp-escape-percent (string)
  (when string
    (replace-regexp-in-string "%" "%%" string)))

(defun picp-header-dir ()
  (if picp-header-full-path
      ;; abbreviate-file-name substitutes the users home directory
      ;; with "~".  This do not work if the home directory is a
      ;; symbolic link.  That case is fixed by appending to
      ;; directory-abbrev-alist here.
      (let ((directory-abbrev-alist
             (append directory-abbrev-alist
                     (list (cons (file-truename "~")
                                 (getenv "HOME"))))))
        (abbreviate-file-name (directory-file-name (picp-dir))))
    (file-name-nondirectory (directory-file-name (picp-dir)))))

(defun picp-maybe-kb ()
  (let ((bytes (picp-bytes picp-current)))
    (if bytes
        (picp-kb bytes)
      "")))


(defun picp-bytes-force (pic)
  (or (picp-bytes pic)
      (picp-save-bytes-in-pic pic)))

(defun picp-save-bytes-in-pic (pic)
  (picp-set-bytes pic (picp-file-bytes (picp-absfile pic))))

(defun picp-file-bytes (file)
  (let ((attributes (file-attributes file)))
    (if attributes
        (elt attributes 7)
      (error "File %s do not exist" file))))

(defun picp-kb (bytes)
  (if (<= 1024 bytes)
      (format "%sk" (/ bytes 1024))
    (format "%s" bytes)))

(defun picp-scale-info ()
  (unless (eq picp-scale 100)
    (format "%s%%%%" picp-scale)))

(defun picp-rotation-info ()
  (let ((degrees (picp-rotation picp-current)))
    (unless (zerop degrees)
      (format "%s°" (truncate degrees)))))

(defun picp-format-tags (tags)
  (if (stringp tags)
      (picp-format-tags (picp-tags-string-to-list tags))
    (when tags
      (cl-case picp-tags-style
        (:org (format ":%s:" (mapconcat #'symbol-name tags ":")))
        (t (format "(%s)" (mapconcat #'symbol-name tags " ")))))))

(defun picp-filter-info ()
  (when picp-filter
    (format "[filter: %s %s/%s]"
            (picp-format-tags picp-filter)
            (or picp-filter-index "?")
            (cond (picp-filter-match-count-done
                   picp-filter-match-count)
                  ((null picp-filter-match-count)
                   "?")
                  ((zerop picp-filter-match-count)
                   "?")
                  (t
                   (format "%s+" picp-filter-match-count))))))


;;; Hook functions

(defun picp-cleanup-hooks ()
  (remove-hook 'window-size-change-functions
               #'picp-window-size-change-function)
  (remove-hook 'buffer-list-update-hook
               #'picp-maybe-update-keymap)
  (remove-hook 'buffer-list-update-hook
               #'picp-maybe-rescale))

(defun picp-window-size-change-function (frame)
  (when picp-adapt-to-window-size-change
    (dolist (window (window-list frame 'no-minibuffer))
      (when (eq (get-buffer picp-buffer) (window-buffer window))
        (with-selected-window window
          (with-current-buffer picp-buffer
            (unless (equal picp-window-size (picp-save-window-size))
              (picp-update-buffer))))))))

(defun picp-maybe-update-keymap ()
  (and picp-keystroke-alist
       (symbolp picp-keystroke-alist)
       (get-buffer picp-buffer)
       (eq (current-buffer) (get-buffer picp-buffer))
       (not (eq (symbol-value picp-keystroke-alist)
                picp-old-keystroke-alist))
       (picp-update-keymap)))

(defun picp-maybe-rescale ()
  (let ((buffer (get-buffer picp-buffer)))
    (and buffer
         picp-adapt-to-window-size-change
         (eq (current-buffer) buffer)
         (eq (window-buffer) buffer)
         ;; It is important to check and save window size here.
         ;; Otherwise the call to picp-old-update-buffer may trigger an
         ;; infinite loop via buffer-list-update-hook.
         (not (equal picp-window-size (picp-save-window-size)))
         (picp-update-buffer))))

(defun picp-delete-trashcan ()
  (when picp-trashcan
    (delete-directory picp-trashcan t)
    (setq picp-trashcan nil)))



;;; Debug functions

;; PENDING call at start, finish and interrupt of idle functions...
(defun picp-debug2 (format &optional args)
  (apply #'message format args))
;; (update-header-debug-thing...)
;; (scroll-message-buffer-if-visible...))


(defun picp-debug (s format &rest args)
  (when picp-debug
    (setq picp-sum (time-add picp-sum s))
    (setq picp-header-text (format "(%s %s (%s))"
                                   (apply #'format format args)
                                   (picp-sec-string s)
                                   (picp-sec-string picp-sum)))
    (message "picp-debug: %s" picp-header-text)))

(defun picp-dump ()
  "Print some picpocket variables."
  (interactive)
  (picp-command
    (picp-print 'picp-list)
    (picp-print 'picp-current)
    (picp-print 'picp-index)
    (picp-print 'picp-list-length)
    (message "")
    (view-echo-area-messages)
    t))

(defun picp-print (var)
  (let ((value (symbol-value var)))
    (and (listp value)
         (picp-pic-p (car value))
         (setq value (picp-simplify-list value)))
    (message "%-20s%s"
             var
             (with-temp-buffer
               (pp value (current-buffer))
               (goto-char (point-min))
               (forward-line)
               (indent-rigidly (point) (point-max) 20)
               (buffer-string)))))

(defun picp-simplify-list (&optional list)
  "Print the LIST or current picture list without the prev links.
With the prev links it is harder to follow the list."
  (cl-loop for pic in (or list picp-list)
           for copy = (copy-picp-pic pic)
           do (setf (picp-pic-prev copy) :prev)
           collect copy))

(defun picp-dump-list (&optional list)
  (pp (picp-simplify-list list)))

(defun picp-action-past-tense (action)
  (cl-case action
    (add-tag "Tagged")
    (copy "Copied")
    (move "Moved")
    (rename "Renamed")
    (hardlink "Hard linked")))

;; PENDING - just for comparing between single-linked and
;; double-linked list.
(defun picp-backwards ()
  "Move backwards without using double-linked list."
  (interactive)
  (picp-command
    (when (eq picp-list picp-current)
      (user-error "No previous pic"))
    (let* ((list picp-list)
           (time (picp-time-string
                   (while (not (eq picp-current (cdr list)))
                     (setq list (cdr list))))))
      (picp-list-set-pos (make-picp-pos :current list
                                        :index (1- picp-index)))
      (message "Back %s" time))))


(provide 'picpocket)

;;; picpocket.el ends here
