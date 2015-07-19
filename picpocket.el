;;; picpocket.el --- Image viewer -*- lexical-binding: t; coding: utf-8-unix -*-

;; Copyright (C) 2015 Johan Claesson
;; Author: Johan Claesson <johanclaesson@bredband.net>
;; Maintainer: Johan Claesson <johanclaesson@bredband.net>
;; Created: 2015-02-16
;; Time-stamp: <2015-07-19 16:33:54 jcl>
;; Version: 16

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
;; * Customizing keystrokes for tagging and file operations.
;; * A simple slide show mode.
;;
;; Main entry point
;; ----------------
;;
;; Command: picpocket
;;  View the pictures in the current directory.
;;
;; Main keybindings
;; ----------------
;;
;; Space     - Next picture.
;; BackSpace - Previous picture.
;; r         - Rename picture file.
;; c         - Copy picture file.
;; k         - Delete picture file.
;; t         - Edit tags.
;; s         - Slide-show mode.
;; [         - Rotate counter-clockwise.
;; ]         - Rotate clockwise.
;; +         - Scale in.
;; -         - Scale out.
;; e         - Customize keystrokes (see below).
;; TAB f     - Toggle full-screen.
;; TAB r     - Toggle recursive inclusion of pictures in sub-directories.
;;
;; With prefix argument many of the commands will operatate on all the
;; pictures in the current list instead of just the current picture.
;;
;; Disclaimer
;; ----------
;;
;; Picpocket will secretly do stuff in the background with idle
;; timers.  This includes to load upcoming pictures into the image
;; cache.  The intention is that they should block Emacs for so short
;; periods that it is not noticable.  But if you want to get rid of
;; them for sure just kill the picpocket buffer.
;;
;; Picpocket is to be considered beta software.  Keybindings,
;; variables and such may change in future versions.  Tag database
;; file format will remain backwards compatible though.
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
;; (defcustom my-picp-alist
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
;;     (?B copy "~/backup"))
;;   "Tic doc."
;;   :group 'picpocket
;;   :set (lambda (symbol value)
;;          (set symbol value)
;;          (picp-update-keymap)))
;;
;; (setq picp-keystroke-alist 'my-picp-alist)
;; (put 'my-picp-alist 'risky-local-variable t)
;;
;; Digits and capital letters with no modifiers is reserved for these
;; kind of user keybindings.
;;
;; It is recommended to set `picp-keystroke-alist' to a symbol as
;; above.  That makes the command `picp-edit-keystrokes' (bound to `e'
;; in picpocket buffer) jump to your definition for convenience.
;;
;; It is recommended to define `my-picp-alist' (or whatever you call
;; it) with defcustom and the :set function above.  That way you only
;; have to type M-C-x once after editing and it will have effect
;; immediately.
;;
;; Finally flagging `my-picp-alist' as a risky local variable is also
;; recommended for paranoid security reasons.
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
;; file name for each entry of tags.  The command `picp-db-update'
;; will go through the database and offer to recover such lost
;; associations.
;;
;; If you change the file-name and the file content at the same time
;; there is no way to recover automatically.

;;; Code:

;; TODO.
;; * Use same default sort order as dired?
;; * Why is look ahead slower in version 13 than in 12.x?
;; * Worthwhile to delay calculation of picp-bytes to idle timer?
;; * Remove all &optional before pic.
;; * defvar -> defcustom where appropriate.
;; * Command to show all pictures in database with certain tags.
;; * Logical operators for filters
;;   filter: !bw manga
;;   ! means NOT, space means AND, comma means OR.
;; * Another hash table mapping truenames to sha.
;;   Filter will be slow without it.  Store also mtime and trust cache
;;   while mtime stays the same.
;; ** This will make it cheap to recover lost connections like
;;    `picp-db-update' for single files.
;; * Move and delete commands are not executed immediately.  They are
;;   stored in a action slot in struct pic.  All actions are executed
;;   on ### or so.  Maybe the action slot should be a list of
;;   actions.
;; ** Optionally do execute the actions eventually.  There is a queue
;;    of 10 or so actions.
;; ** Command to view all pending actions.  Preferably with
;;    thumbnails.
;; ** Undo command that undoes one pending action at a time.
;; * PENDING comments.
;; * Idle timer that computes index/max for filter and puts it in
;;   header line.
;; * If target starts with / or ~ it will always be absolute.

;; NICE
;; * Smoother movement commands (when picture is bigger than window)
;; * The tags for a filename is it's directory name plus the picp-tags.
;; * Underscores and dashes should be ignored in tag names.
;; * Activate picp-look-ahead-more
;; ** Maybe implement resume timers for it
;; * See quit-window.  Should quit-restore delete full-screen frame?
;; * Command with ido over the defined shortcuts.
;; * Generate doc for generated commands.
;; * Call elpa/eimp for permanent rotation etc.
;; * picp-toggle-specify-subdir
;;   When moving to a dir with subdirs ask if it shall go into any of these.
;;   picp-include-subdir-regexp
;;   picp-exclude-subdir-regexp
;; * / -> keymap auto-generated by sub-directories.
;;        f moves to subdir "foo" and so on.
;;   // -> select subdir from menu.
;; * Look-ahead should also read the sizes of all files.
;; * Multiple simultaneous picpocket buffers.
;;   Universal arg to picpocket command will stop it from killing old buffer.
;; * Cache mystery with black full-screen frame.
;; ** Test with all black frames.
;; ** C debug.
;; ** imagemagick-render-type.
;; * picp-slideshow-mode-map
;; ** Commands for increasing/decreasing speed.
;; ** ? shows commands and the current speed.
;; * i -> buffer with simple index of files.
;; * picp-update-keymap creates keymap strings:
;;   [m manga - s scifi - d dragon]
;; * Break out picp-db to library of it's own: jhash.el
;;   (jhash-init :picp picp-test-dir picp-db-format)
;;   (defsubst picp-db-put (sha value)
;;   (jhash-put :picp sha value))
;; * Option: When a pic tagged with X is moved to a directory with sub-directory
;;   X the pic will move to that sub-directory.
;; * Insert picp-keystroke-alist template in .emacs if undefined.
;; * Alternative to picp-adapt-to-window-size-change.
;;   Save "drawing" closure in variable.  Normally this is
;;   picp-update-buffer.  The only other currently valid value is
;;   picp-compare.  This will make picp-compare also adapt to window
;;   size changes.  Not worth the effort for only picp-compare but if
;;   more different drawing functions appears then maybe.

(eval-when-compile
  (require 'time-date)
  (require 'ido))

(require 'cl-lib)
(require 'dired)



;;; Options

(defvar picp-keystroke-alist nil
  "Symbol with a alist of picpocket keystrokes.
Elements in the alist have the form (KEY ACTION TARGET).

KEY is a single character, a string accepted by `kbd' or a vector
accepted by `define-key'.

ACTION is tag, move, copy or hardlink.

TARGET is a string.  For ACTION tag it is the tag to add.  For
the other actions it is the destination directory.

Note that you have to call `picp-update-keymap' to make the
changes of variable `picp-keystroke-alist' have effect.  One way
to do this is to define it with `defcustom' like this:

 (defcustom my-picp-alist
   '((?1 tag \"bad\")
     (?2 tag \"sigh\")
     (?3 tag \"good\")
     (?4 tag \"great\")
     (?5 tag \"awesome\"))
   \"Tic doc.\"
   :group 'picpocket
   :set (lambda (symbol value)
          (set symbol value)
          (picp-update-keymap)))

 (setq picp-keystroke-alist 'my-picp-alist)
 (put 'my-picp-alist 'risky-local-variable t)")

(defgroup picpocket nil "Picture viewer."
  :group 'picpocket)
(defcustom picp-fit :x-and-y
  "Fit picture size to window when non-nil."
  :type '(choice (const :tag "Fit to both width and height" :x-and-y)
                 (const :tag "Fit to width" :x)
                 (const :tag "Fit to height" :y)
                 (const :tag "Show picture in it's natural size" nil)))
(defcustom picp-scale 100
  "Picture scaling in percent."
  :type 'integer)

(defvar picp-header-line-format '(:eval (picp-header-line)))
(defvar picp-header-full-path nil)
(defvar picp-look-ahead-max 5)
(defvar picp-confirm-delete (not noninteractive))
(defvar picp-tags-style 'list)
(defvar picp-demote-warnings nil)
(defvar picp-backdrop-command nil)
(defvar picp-default-backdrop-commands
  '(("display" . "-window root")
    ("feh" . "--bg-file")
    ("hsetroot" . "-tile")
    ("xsetbg")))
(defvar picp-header t)
(defvar picp-recursive nil)
(defvar picp-follow-symlinks t
  "If non-nil follow symlinks while traversing directories recursively.
Symlinks that points to picture files are always followed.
This option concerns only symlinks that points to directories.")
(defvar picp-dst-dir "~/")
(defvar picp-dst-dir-is-cwd t)


;;; Internal variables

(defconst picp-version 16)
(defconst picp-buffer "*picpocket*")

(defvar picp-frame nil)
(defvar picp-old-frame nil)
(defvar picp-last-action nil)
(defvar picp-last-arg nil)
(defvar picp-last-arg-is-dir nil)
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
This is kept for the benefit of timer functions that do not
necessarily run with the picpocket window selected.")
(defvar picp-header-text "")

;; Variables displayed in the header-line must be marked as risky.
(dolist (symbol '(picp-filter
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
need to be non-zero when printing it.  If it is nil Emacs will
hang.")
(defvar picp-length nil)
(defvar picp-current nil)
(defvar picp-index nil
  "The `picp-index' is starting from 1 (incompatible with elt).")

(put 'picp-index 'risky-local-variable t)
(put 'picp-length 'risky-local-variable t)
(put 'picp-current 'risky-local-variable t)

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

;; Great macro from slime.el.
(cl-defmacro picp-when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY.
\(fn (VAR VALUE) &rest BODY)"
  (declare (debug ((symbolp form) body))
           (indent defun))
  `(let ((,var ,value))
     (when ,var ,@body)))

(defmacro picp-time-string (&rest forms)
  (declare (indent defun))
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
  (declare (indent defun))
  (let ((before (make-symbol "before"))
        (rc (make-symbol "rc")))
    `(let ((,before (current-time))
           (,rc (progn ,@forms)))
       (list ,rc (time-since ,before)))))

(defmacro picp-clock (&rest body)
  (let ((thing (caar body)))
    `(picp-clock-thing ',thing ,@body)))

(defmacro picp-clock-thing (thing &rest body)
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
  (declare (debug ((symbolp form) body))
           (indent defun))
  `(let (picp-clock-alist)
     (prog1
         (progn ,@body)
       (picp-clock-report ,title))))

;;; Picp mode

(define-derived-mode picp-mode special-mode "picpocket"
  (buffer-disable-undo)
  ;; Ensure imagemagick is preferred.
  (unless (eq 'imagemagick (cdar image-type-file-name-regexps))
    (let ((entry (rassq 'imagemagick image-type-file-name-regexps)))
      (setq-local image-type-file-name-regexps
                  (cons entry
                        (delete entry
                                (cl-copy-list image-type-file-name-regexps))))))
  (setq-local image-type-header-regexps nil)
  (picp-db-init)
  (setq cursor-type nil
        truncate-lines t
        auto-hscroll-mode nil
        vertical-scroll-bar nil
        left-fringe-width 0
        right-fringe-width 0)
  ;; Call set-window-buffer to update the fringes.
  (set-window-buffer (selected-window) (current-buffer))
  (setq header-line-format (when picp-header
                             picp-header-line-format))
  (when (eq (selected-frame) picp-frame)
    (setq mode-line-format nil))
  (picp-update-keymap)
  (add-hook 'kill-buffer-hook #'picp-save-journal nil t)
  (add-hook 'window-size-change-functions #'picp-window-size-change-function)
  (picp-init-timers))




(let ((map (make-sparse-keymap))
      (toggle-map (make-sparse-keymap
                   (concat "Toggle: [f - fullscreen]"
                           " [h - header]"
                           " [d - destination]"
                           " [r - recursive]"
                           " [l - follow symlinks]"
                           " [D - debug]"))))
  (define-key toggle-map [?f] #'picp-toggle-fullscreen-frame)
  (define-key toggle-map [?d] #'picp-toggle-dst-dir-is-cwd)
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
  (define-key map [?b] #'picp-fit-to-width-and-height)
  (define-key map [?w] #'picp-fit-to-width)
  (define-key map [?h] #'picp-fit-to-height)
  (define-key map [?n] #'picp-no-fit)
  (define-key map [??] #'picp-help)
  (define-key map [left] #'scroll-right)
  (define-key map [right] #'scroll-left)
  (define-key map [(control ?b)] #'scroll-right)
  (define-key map [(control ?f)] #'scroll-left)
  (define-key map [?j] #'picp-jump)
  (define-key map [??] #'picp-help)
  (setq picp-mode-map map))

;;; Entry points.

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
                              (picp-path))
                             (t nil))))
    (picpocket-dir default-directory selected-file)))

(defun picpocket-dir (dir &optional selected-file)
  (setq picp-entry-function 'picpocket-dir
        picp-entry-args (list dir))
  (let ((files (picp-file-list dir)))
    (picp-create-buffer files selected-file dir)))

(defun picpocket-files (files &optional selected-file)
  (setq picp-entry-function 'picpocket-files
        picp-entry-args (list files))
  (picp-create-buffer files selected-file))


;;; Picpocket mode commands.

(defun picp-rotate-counter-clockwise (&optional arg)
  "Display the current picture rotated 90 degrees to the left.
This command do not change the picture file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picp-rotate -90.0 arg))

(defun picp-rotate-clockwise (&optional arg)
  "Display the current picture rotated 90 degrees to the right.
This command do not change the picture file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picp-rotate 90.0 arg)
  (picp-update-buffer))

(defun picp-rotate-counter-clockwise-10-degrees (&optional arg)
  "Display the current picture rotated 10 degrees to the left.
This command do not change the picture file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picp-rotate -10.0 arg))

(defun picp-rotate-clockwise-10-degrees (&optional arg)
  "Display the current picture rotated 10 degrees to the right.
This command do not change the picture file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picp-rotate 10.0 arg)
  (picp-update-buffer))

(defun picp-reset-rotation ()
  "Display the current picture as is without any rotation."
  (interactive)
  (picp-set-rotation picp-current 0.0)
  (picp-update-buffer))

(defun picp-rotate (delta arg)
  (let ((degrees (if arg
                     (float (read-number "Set rotation in degrees"
                                         (picp-rotation)))
                   (+ (picp-rotation) delta))))
    (picp-set-rotation picp-current degrees)
    (picp-update-buffer)))


(defun picp-scale-in (arg)
  "Zoom in 10%.
With prefix arg (ARG) read scale percent in minibuffer."
  (interactive "P")
  (if arg
      (setq picp-scale (read-number "Scale factor: " picp-scale))
    (picp-alter-scale 10))
  (picp-update-buffer))

(defun picp-scale-out (arg)
  "Zoom out 10%.
With prefix arg (ARG) read scale percent in minibuffer."
  (interactive "P")
  (if arg
      (setq picp-scale (read-number "Scale factor: " picp-scale))
    (picp-alter-scale -10))
  (picp-update-buffer))

(defun picp-reset-scale ()
  "Reset the scale to 100%."
  (interactive)
  (setq picp-scale 100)
  (message "Restore the scale to 100%%.")
  (picp-update-buffer))

(defun picp-fit-to-width-and-height ()
  "Fit the picture to both width and height of window.
Fitting is done before applying the scaling factor.  That is, it
will fit only when the scaling is the default 100%.  The scaling
can be restored to 100% by typing \\[picp-reset-scale] \(for
`picp-reset-scale')."
  (interactive)
  (setq picp-fit :x-and-y)
  (message "Fit picture to both width and height")
  (picp-update-buffer))

(defun picp-fit-to-width ()
  "Fit the picture to the width of window.
Fitting is done before applying the scaling factor.  That is, it
will fit only when the scaling is the default 100%.  The scaling
can be restored to 100% by typing \\[picp-reset-scale] \(for
`picp-reset-scale')."
  (interactive)
  (setq picp-fit :x)
  (message "Fit picture to width")
  (picp-update-buffer))

(defun picp-fit-to-height ()
  "Fit the picture to the height of window.
Fitting is done before applying the scaling factor.  That is, it
will fit only when the scaling is the default 100%.  The scaling
can be restored to 100% by typing \\[picp-reset-scale] \(for
`picp-reset-scale')."
  (interactive)
  (setq picp-fit :y)
  (message "Fit picture to height")
  (picp-update-buffer))

(defun picp-no-fit ()
  "Do not fit the picture to the window."
  (interactive)
  (setq picp-fit nil)
  (message "Do not fit picture to window size")
  (picp-update-buffer))


(defun picp-set-backdrop ()
  "Attempt to install the current picture as desktop backdrop."
  (interactive)
  (setq picp-backdrop-command (or picp-backdrop-command
                                  (picp-default-backdrop-command)))
  (unless picp-backdrop-command
    (error (concat "Command to set backdrop not found."
                   " Set picp-backdrop-command or install %s")
           (picp-default-backdrop-commands-string)))
  (let* ((words (split-string picp-backdrop-command))
         (cmd (car words))
         (file (picp-path))
         (args (append (cdr words) (list file))))
    (with-temp-buffer
      (unless (zerop (apply #'call-process cmd nil t nil args))
        (setq picp-backdrop-command nil)
        (error "Command \"%s %s\" failed with output \"%s\""
               words file (buffer-string))))))

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
  (setq picp-debug (not picp-debug))
  (message "Picpocket debug is %s." (if picp-debug "on" "off")))

(defun picp-toggle-recursive ()
  "Toggle recursive inclusion of sub-directories.
Directories whose name starts with a dot will not be traversed.
However picture files whose name starts with dot will be
included."
  (interactive)
  (setq picp-recursive (not picp-recursive))
  (picp-revert)
  (message (if picp-recursive
               "Recursively include pictures in subdirectories."
             "Only show pictures in current directory.")))

(defun picp-toggle-follow-symlinks ()
  "Toggle whether to follow symlinks while recursively traversing directories.
Symlinks that points to picture files are always followed.
This command concerns only symlinks that points to directories."
  (interactive)
  (setq picp-follow-symlinks (not picp-follow-symlinks))
  (picp-revert)
  (message (if picp-follow-symlinks
               "Follow symlinks."
             "Do not follow symlinks.")))

(defun picp-dired-up-directory ()
  "Enter Dired mode in the parent directory."
  (interactive)
  (let ((dir default-directory))
    (quit-window)
    (dired (file-name-directory (directory-file-name dir)))
    (dired-goto-file dir)))


(defun picp-quit ()
  "Quit picpocket."
  (interactive)
  (picp-disable-fullscreen)
  (picp-save-journal)
  (quit-window))

(defun picp-disable-fullscreen ()
  (when (picp-fullscreen-p)
    (picp-toggle-fullscreen-frame)))

(defun picp-toggle-dst-dir-is-cwd (&optional ask-for-dir)
  "Toggle the destination for file operations.

File operations are move, copy and hardlink.  Either the
destination is relative to the current directory.  Or it is
relative the value of variable `picp-dst-dir'.

With prefix arg ask for a directory and set variable
`picp-dst-dir' to that.  Calling from Lisp with the argument
ASK-FOR-DIR non-nil will also do that."
  (interactive "P")
  (if ask-for-dir
      (setq picp-dst-dir-is-cwd nil
            picp-dst-dir
            (read-directory-name "Destination dir: "))
    (setq picp-dst-dir-is-cwd
          (not picp-dst-dir-is-cwd)))
  (message "Destination directory is relative to %s."
           (if picp-dst-dir-is-cwd
               "the current directory"
             picp-dst-dir)))



(defun picp-edit-keystrokes ()
  "Move to definition of variable `picp-keystroke-alist'.
To use this command you must set variable `picp-keystroke-alist'
to a variable symbol.  The purpose of this command is to be
able to quickly move to the definition and edit keystrokes."
  (interactive)
  (unless picp-keystroke-alist
    (user-error
     "You need to set picp-keystroke-alist for this command to work"))
  (find-variable-other-window picp-keystroke-alist)
  (goto-char (point-at-eol)))

(defun picp-slideshow ()
  "Start slide-show."
  (interactive)
  (while (not (input-pending-p))
    (picp-next)
    (sit-for 8))
  (message "End of slideshow."))

(defun picp-visit-file ()
  "Open the current picture in default mode (normally `image-mode')."
  (interactive)
  (find-file (picp-path)))

(defun picp-toggle-fullscreen-frame ()
  "Toggle use of fullscreen frame.

The first call will show the picpocket buffer in a newly created
frame in fullscreen mode.  It is meant to only show the picpocket
buffer (but this is not enforced).  The second call will delete
this frame and go back to the old frame."
  (interactive)
  (if (picp-fullscreen-p)
      (progn
        (delete-frame picp-frame)
        (setq picp-frame nil)
        (picp-select-frame picp-old-frame)
        (setq mode-line-format (default-value 'mode-line-format))
        (picp-update-buffer))
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
    (picp-update-buffer)))

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
  (let ((next (picp-next-pos)))
    (if next
        (picp-set-pos next)
      (picp-no-file "next")))
  (picp-update-buffer))

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
  (let ((prev (picp-previous-pos)))
    (if prev
        (picp-set-pos prev)
      (picp-no-file "previous")))
  (picp-update-buffer))

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
  (picp-set-pos (picp-first-pos))
  (unless (picp-filter-match-p picp-current)
    (let ((next (picp-next-pos)))
      (if next
          (picp-set-pos next)
        (picp-no-file))))
  (picp-update-buffer))

(defun picp-first-pos ()
  (make-picp-pos :current picp-list
                 :index 1
                 :filter-index (if (picp-filter-match-p picp-list)
                                   1
                                 0)))

(defun picp-end ()
  "Move to the last picture in the current list."
  (interactive)
  (picp-set-pos (picp-last-pos))
  (unless (picp-filter-match-p picp-current)
    (let ((prev (picp-previous-pos)))
      (if prev
          (picp-set-pos prev)
        (picp-no-file))))
  (picp-update-buffer))

(defun picp-last-pos ()
  (cl-loop for pic on picp-current
           for index = picp-index then (1+ index)
           when (null (cdr pic))
           return (make-picp-pos :current pic
                                 :index index)))

(defun picp-delete-file ()
  "Permanently delete the current picture file."
  (interactive)
  (let ((file (picp-file)))
    (when (if picp-confirm-delete
              (picp-y-or-n-p "Delete file %s from disk? " file)
            t)
      (picp-delete)
      (message "%s is no more." file)
      (picp-update-buffer))))

(defun picp-repeat ()
  "Repeat the last repeatable action.
The repeatable actions are:
1. Move/copy/hardlink the current picture to a directory.
2. Add a tag to the current picture."
  (interactive)
  (if picp-last-action
      (progn
        (picp-action picp-last-action picp-last-arg picp-last-arg-is-dir)
        (picp-update-buffer))
    (user-error "No repeatable action have been done")))


(defun picp-dired ()
  "Visit the current directory in `dired-mode'."
  (interactive)
  (if picp-current
      (let ((dir (picp-dir))
            (file (picp-path)))
        (dired default-directory)
        (when (and (equal dir (file-truename default-directory))
                   (file-exists-p file))
          (dired-goto-file file)))
    (dired default-directory)))

(defun picp-toggle-header ()
  "Toggle the display of the header line."
  (interactive)
  (setq picp-header (not picp-header)
        header-line-format (when picp-header
                             picp-header-line-format))
  (force-mode-line-update))


(defun picp-revert ()
  "Revert the current picpocket buffer.
Update the current list of pictures.
When called from Lisp return the new picpocket buffer."
  (interactive)
  ;; For now assume selected-file is the second arg to all possible
  ;; picp-entry-functions.
  (apply picp-entry-function (append picp-entry-args
                                     (when picp-current
                                       (list (picp-path))))))

(defun picp-rename (dst)
  "Edit the filename of current picture.
When called from Lisp DST is the destination directory."
  (interactive (list (progn
                       (when (boundp 'ido-read-file-name-non-ido)
                         (add-to-list 'ido-read-file-name-non-ido
                                      #'picp-rename))
                       (read-file-name "To: " (picp-path)))))
  (picp-action 'move dst)
  (picp-update-buffer))

(defun picp-move (all)
  "Move current picture to another directory.
With prefix arg (ALL) move all pictures in the current list."
  (interactive "P")
  (if all
      (picp-move-all (read-directory-name "Move all pictures to: "
                                          (picp-dst-dir)))
    (picp-action 'move (read-directory-name "Move to: " (picp-dst-dir))))
  (picp-update-buffer))

(defun picp-move-all (dst)
  (let ((pic picp-list))
    (while pic
      (when (picp-filter-match-p pic)
        (picp-action 'move dst t pic))
      (setq pic (cdr pic)))
    (message "Moved all to %s." dst)))

(defun picp-copy (all)
  "Copy the current picture to another directory.
With prefix arg (ALL) copy all pictures in the current list."
  (interactive "P")
  (if all
      (picp-copy-all (read-directory-name "Copy all pictures to: "
                                          (picp-dst-dir)))
    (picp-action 'copy (read-directory-name "Copy to: " (picp-dst-dir)))))

(defun picp-copy-all (dst)
  (picp-mapc (lambda (pic)
               (picp-action 'copy dst t pic)))
  (message "Copied all to %s." dst))

(defun picp-hardlink (all)
  "Make a hard link to the current picture in another directory.
With prefix arg (ALL) hard link all pictures in the current list."
  (interactive "P")
  (if all
      (picp-hardlink-all (read-directory-name "Hard link all pictures to: "
                                               (picp-dst-dir)))
    (picp-action 'hardlink (read-directory-name "Hard link to: "
                                                 (picp-dst-dir)))))

(defun picp-hardlink-all (dst)
  (picp-mapc (lambda (pic)
               (picp-action 'hardlink dst t pic)))
  (message "Hard linked all to %s." dst))


(defun picp-move-by-keystroke (all)
  "Move the current picture.
The destination directory is determined by a keystroke that is
lookup up in the variable `picp-keystroke-alist'.
With prefix arg (ALL) move all pictures in the current list."
  (interactive "P")
  (if all
      (let ((dst (picp-read-key "directory to move all pictures to")))
        (picp-mapc (lambda (pic)
                     (picp-action 'move dst t pic))))
    (picp-action 'move (picp-read-key "directory to move to")))
  (picp-update-buffer))

(defun picp-copy-by-keystroke (all)
  "Copy the current picture.
The destination directory is determined by a keystroke that is
lookup up in the variable `picp-keystroke-alist'.
With prefix arg (ALL) copy all pictures in the current list."
  (interactive "P")
  (if all
      (let ((dst (picp-read-key "directory to copy all pictures to")))
        (picp-mapc (lambda (pic)
                     (picp-action 'copy dst t pic))))
    (picp-action 'copy (picp-read-key "directory to copy to"))))

(defun picp-hardlink-by-keystroke (all)
  "Make a hard link to the current picture.
The destination directory is determined by a keystroke that is
lookup up in the variable `picp-keystroke-alist'.
With prefix arg (ALL) hard link all pictures in the current list."
  (interactive "P")
  (if all
      (let ((dst (picp-read-key "directory to link all pictures to")))
        (picp-mapc (lambda (pic)
                     (picp-action 'hardlink dst t pic))))
    (picp-action 'hardlink (picp-read-key "directory to link to"))))


(defun picp-tag-by-keystroke (&optional all)
  "Add a tag to the current picture.
The tag is determined by a keystroke that is looked up in the
variable `picp-keystroke-alist'.

With prefix arg (ALL) the tag is added to all pictures in the
current list.  Type a minus sign (-) before the keystroke to
remove the tag from all pictures instead."
  (interactive "P")
  (if all
      (picp-tag-to-all (picp-read-key-to-add-or-remove-tag nil t))
    (picp-action 'tag (picp-read-key-to-add-or-remove-tag)))
  (picp-update-buffer))

(defun picp-edit-tags (&optional all)
  "Edit the tags associated with current picture.
To enter multiple tags separate them with spaces.

With prefix arg (ALL) enter a single tag and add it to all
pictures in the current list.  If given string begins with a
minus sign (-) then the tag is removed from all pictures
instead."
  (interactive "P")
  (if all
      (picp-tag-to-all
       (read-string "Type tag to add to all files (-tag to remove): "))
    (let* ((old-tags-string (picp-tags-string-to-edit (picp-tags picp-current)))
           (new-tags-string (read-string "Tags: " old-tags-string))
           (new-tag-symbols (mapcar #'intern (split-string new-tags-string))))
      (picp-tags-set picp-current new-tag-symbols)
      (picp-update-buffer)
      (message "Tags for %s is %s."
               (picp-file)
               (if new-tag-symbols
                   new-tag-symbols
                 "cleared")))))

(defun picp-tags-string-to-edit (tags)
  (when tags
    (concat (mapconcat #'symbol-name tags " ") " ")))

(defun picp-set-filter (filter-string)
  "Enter the current picpocket filter.
The filter is a list of tags.  Only pictures with all the tags in
the filter is shown.  To enter multiple tags separate them with
spaces.

When called from Lisp the argument FILTER-STRING is a
space-separated string."
  (interactive (list (read-string "Show only pictures with this tag: ")))
  (picp-do-set-filter (mapcar #'intern (split-string filter-string)))
  (picp-update-buffer))

(defun picp-do-set-filter (filter)
  (setq picp-filter filter
        picp-filter-index nil
        picp-compute-filter-index-from-scratch t
        picp-filter-match-count nil
        picp-filter-match-count-done nil))

(defun picp-filter-match-p (pic)
  (cl-subsetp picp-filter (picp-tags pic)))

(defun picp-no-file (&optional direction)
  (user-error (picp-join "No"
                         direction
                         "file"
                         (when picp-filter
                           (format "match filter %s" picp-filter)))))

(defun picp-set-filter-by-keystroke ()
  "Show only pictures having the tag in the current filter."
  (interactive)
  (picp-set-filter (picp-read-key "filtering tag")))


(defun picp-jump ()
  "Jump to picture specified by file-name or index number."
  (interactive)
  (let ((nr-or-file-name (completing-read "Jump to index or file-name: "
                                          (picp-mapcar 'picp-file))))
    (or (picp-jump-to-index nr-or-file-name)
        (picp-jump-to-file nr-or-file-name))
    (picp-update-buffer)))

(defun picp-jump-to-index (string)
  (when (string-match "^[0-9]+$" string)
    (let ((index (string-to-number string)))
      (picp-when-let (pic (picp-pic-by-index index))
        (picp-set-pos (make-picp-pos :current pic
                                     :index index))
        t))))


(defun picp-pic-by-index (n)
  (and (< 0 n)
       (<= n picp-length)
       (cl-loop for pic on picp-list
                repeat (1- n)
                finally return pic)))

(defun picp-jump-to-file (file)
  (let ((pos-list (picp-pos-list-by-file file)))
    (cond ((null pos-list)
           (user-error "Picture not found (%s)" file))
          ((eq 1 (length pos-list))
           (picp-set-pos (car pos-list)))
          (t
           (let ((prompt (format "%s is available in %s directories.  Select: "
                                 file (length pos-list))))
             (picp-set-pos (picp-select-pos-by-dir pos-list prompt))
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


;;; Pic double-linked list functions.

;; These will call tag handling functions.

(defun picp-make-pic (path)
  (list (make-picp-pic :dir (file-name-directory path)
                       :file (file-name-nondirectory path))))

(defun picp-reset-list ()
  (setq picp-list nil
        picp-length 0)
  (picp-set-pos (make-picp-pos))
  (picp-do-set-filter nil))

(defun picp-set-pos (pos)
  (let ((inhibit-quit t))
    (setq picp-current (picp-pos-current pos)
          picp-index (or (picp-pos-index pos)
                         (picp-calculate-index picp-current))
          picp-filter-index (picp-pos-filter-index pos)
          picp-compute-filter-index-from-scratch (null picp-filter-index))))


(defun picp-calculate-index (&optional current)
  (when picp-list
    (cl-loop for pic on picp-list
             count 1
             until (eq pic (or current picp-current)))))

(defun picp-delete ()
  (let ((file (picp-path)))
    (let ((inhibit-quit t)
          (filter-match (picp-filter-match-p picp-current)))
      (delete-file file)
      (picp-tags-delete-file picp-current file)
      (picp-delete-pic picp-current (list filter-match)))))

(defun picp-delete-pic (&optional pic filter-match-cell)
  (setq pic (or pic
                picp-current))
  (let ((filter-match (if filter-match-cell
                          (car filter-match-cell)
                        (picp-filter-match-p pic))))
    (clear-image-cache (picp-path pic))
    (setq picp-length (1- picp-length))
    (when picp-filter
      (if picp-filter-match-count-done
          (when filter-match
            (cl-decf picp-filter-match-count))
        (setq picp-filter-match-count nil))
      (setq picp-filter-index nil
            picp-compute-filter-index-from-scratch t))
    (if (picp-prev pic)
        (setcdr (picp-prev pic) (cdr pic))
      (setq picp-list (cdr pic)))
    (if (cdr pic)
        (progn
          (picp-set-prev (cdr pic) (picp-prev pic))
          (when (eq picp-current pic)
            (picp-set-pos (make-picp-pos :current (cdr pic)
                                         :index picp-index))))
      (if (picp-prev pic)
          (when (eq picp-current pic)
            (picp-set-pos (make-picp-pos :current (picp-prev pic)
                                         :index (1- picp-index))))
        (picp-reset-list)))))

(defun picp-create-picp-list (files &optional selected-file)
  (picp-reset-list)
  (setq picp-list
        (cl-loop for path in files
                 for dir = (file-truename (file-name-directory path))
                 for file = (file-name-nondirectory path)
                 if (file-exists-p path)
                 collect (make-picp-pic :dir dir
                                        :file file
                                        :bytes (picp-file-bytes path))
                 else do (message "%s do not exist" path)))
  (cl-loop for pic on picp-list
           with prev = nil
           do (picp-set-prev pic prev)
           do (setq prev pic))
  (setq picp-length (length picp-list))
  (unless (and selected-file
               (string-match (picp-picture-regexp) selected-file)
               (cl-loop for pic on picp-list
                        for index = 1 then (1+ index)
                        when (equal selected-file
                                    (concat (picp-dir pic) (picp-file pic)))
                        return (picp-set-pos (make-picp-pos :current pic
                                                            :index index))))
    (picp-set-pos (make-picp-pos :current picp-list
                                 :index 1))))




;;; Tag database interface functions

;; This layer translates from struct picp-pic to a sha1 checksum.
;; This checksum is refered to as sha and is used as index in the
;; database below.

(defun picp-tags (&optional pic)
  (picp-db-tags (picp-sha-via-cache pic)))

(defun picp-tags-set (pic new-tags)
  (let ((match-before (picp-filter-match-p pic)))
    (picp-db-tags-set (picp-sha-via-cache pic)
                      (picp-path pic)
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
  (picp-db-tags-move-file (picp-sha-via-cache pic) old-file new-file))

(defun picp-tags-copy-file (pic new-file)
  (picp-db-tags-copy-file (picp-sha-via-cache pic) new-file))

(defun picp-tags-delete-file (pic deleted-file)
  (picp-db-tags-delete-file (picp-sha-via-cache pic) deleted-file))

(defun picp-add-tag (tag-string &optional pic)
  "Add a tag to current picture or the picture given as argument.
When called from Lisp the first argument TAG-STRING is the tag
and PIC the picture."
  (let* ((pic (or pic picp-current))
         (tag (intern tag-string))
         (tags (picp-tags pic))
         (inhibit-quit t))
    (unless (memq tag tags)
      (picp-tags-set pic (cons tag tags)))))

(defun picp-remove-tag (tag-string &optional pic)
  "Remove a tag from current picture or the picture given as argument.
When called from Lisp the first argument TAG-STRING is the tag
and PIC is the picture."
  (let* ((pic (or pic picp-current))
         (tag (intern tag-string))
         (tags (picp-tags pic))
         (inhibit-quit t))
    (when (memq tag tags)
      (picp-tags-set pic (delq tag tags)))))

(defun picp-tag-to-all (tag-string)
  "Add a tag (TAG-STRING) to all pictures in current picpocket buffer.
If tag starts with minus remove tag instead of add."
  (cond ((string-match "\\`[:space:]*\\'" tag-string)
         (message "Empty string, no action."))
        ((eq (elt tag-string 0) ?-)
         (let ((tag (substring tag-string 1)))
           (cl-loop for pic on picp-list
                    do (picp-remove-tag tag pic))
           (message "Tag %s removed from all." tag)))
        (t
         (cl-loop for pic on picp-list
                  do (picp-add-tag tag-string pic))
         (message "All tagged with %s." tag-string))))


(defun picp-sha-via-cache (pic)
  "Return the sha1 checksum of PIC.
The checksum will be computed if not already available.
Also, if there is a matching entry in the database with tags
then the file of PIC will be added to that entry."
  (or (picp-sha pic)
      (picp-save-sha-in-pic-and-db pic)))

(defun picp-save-sha-in-pic-and-db (pic)
  (let* ((file (picp-path pic))
         (sha (picp-sha1sum file))
         (inhibit-quit t))
    (picp-set-sha pic sha)
    (picp-db-tags-add-file sha file)
    sha))

(defun picp-sha1sum (file)
  (if (executable-find "sha1sum")
      (with-temp-buffer
        (call-process "sha1sum" nil t nil file)
        (buffer-substring 1 41))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (sha1 (current-buffer)))))

;;; Tag database internal functions.

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
  (declare (indent defun))
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
        (picp-db-put sha (list :files (cl-union (list file) files)
                               :tags new-tags))
      (when plist
        (picp-db-remove sha)))))

(defun picp-db-tags-move-file (sha old-file new-file)
  (picp-with-db sha (plist files)
    (when plist
      (let ((new-files (cl-union (list new-file)
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
(defvar picp-db)

;;;###autoload
(defun picp-db-update ()
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
  (switch-to-buffer "*picpocket db update*")
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
        (picp-bold "No files with changed sha1 checksum found.\n\n")
      (let ((n (length sha-changed)))
        (picp-db-update-command [?s]
          (lambda ()
            (picp-bold (concat "The following %s file%s have"
                               " changed %s sha1 checksum.\n")
                       n (picp-plural-s n) (picp-plural-its-their n))
            (insert "Type ")
            (picp-bold "s")
            (insert " to update picpocket database with the new"
                    " sha1 checksum values.\n\n")
            (picp-insert-file-list sha-changed))
          (lambda ()
            (picp-update-sha sha-changed)
            (picp-bold "Sha1 checksums for %s file%s were updated.\n"
                       n (picp-plural-s n))))))

    (if (null redundant-file-missing)
        (picp-bold "No missing redundant files.\n\n")
      (let ((n (length redundant-file-missing)))
        (picp-db-update-command [?r]
          (lambda ()
            (picp-bold (concat "The following %s redundant file name%s"
                               " were not found on disk.\n")
                       n (picp-plural-s n))
            (insert "Their database entries contains at least one other"
                    " file that do exist.\n"
                    "Type ")
            (picp-bold "r")
            (insert " to remove these file names from the picpocket database.\n"
                    "(Their database entries will not be removed.)\n\n")
            (picp-insert-file-list redundant-file-missing))
          (lambda ()
            (picp-remove-file-names-in-db redundant-file-missing)
            (picp-bold "Removed %s missing redundant file name%s from database."
                       n (picp-plural-s n))))))

    (if (null unique-file-missing)
        (picp-bold "No missing unique files.\n\n")
      (let ((n (length unique-file-missing)))
        (picp-db-update-command [?u]
          (lambda ()
            (picp-bold (concat "The following %s unique file name%s"
                               " were not found on disk.\n")
                       n (picp-plural-s n))
            (insert "Their database entries do not contain any"
                    " existing files.\n"
                    "Type ")
            (picp-bold "u")
            (insert " to remove these entries from the picpocket database.\n"
                    "(Their database entries will be removed.)\n\n")
            (picp-insert-file-list unique-file-missing))
          (lambda ()
            (picp-remove-file-names-in-db unique-file-missing)
            (picp-bold (concat "Removed %s missing unique file name%s"
                               " and their entries from database.")
                       n (picp-plural-s n))))))

    (goto-char (point-min))
    (picp-db-mode)))

(defun picp-db-update-command (key text-function command-function)
  "Create a command and bind it to KEY.

The TEXT-FUNCTION will be called immediately and is supposed to
insert some text describing what COMMAND-FUNCTION does.  When KEY
is typed that text will be deleted and the COMMAND-FUNCTION will
be called."
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
                (picp-db-put new-sha (list :files (cl-union (list file) files)
                                           :tags (cl-union tags new-tags))))
           do (picp-with-db sha (plist files)
                (let ((remaining-files (delete file files)))
                  (if remaining-files
                      (picp-db-put sha (plist-put plist :files remaining-files))
                    (picp-db-remove sha))))))

(defun picp-remove-file-names-in-db (missing-files)
  (cl-loop for (file ignored sha) in missing-files
           do (picp-with-db sha (plist files)
                (let ((new-files (delete file files)))
                  (if new-files
                      (picp-db-put sha (plist-put plist :files new-files))
                    (picp-db-remove sha))))))

(defun picp-bold (format &rest args)
  (insert (propertize (apply #'format format args)
                      'face 'bold
                      'font-lock-face 'bold)))

(defun picp-insert-file-list (list)
  (dolist (entry list)
    (insert "  "
            (picp-join (car entry)
                       (picp-format-tags (cadr entry)))
            "\n")))

(define-derived-mode picp-db-mode special-mode "picpocket-db"
  (define-key picp-db-mode-map [?g] #'picp-db-update)
  (setq truncate-lines t))

(defun picp-db-traverse ()
  (picp-db-init)
  (let (sha-changed
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
                                   unique-file-missing))))))
             picp-db)
    (list (cons :sha-changed sha-changed)
          (cons :unique-file-missing unique-file-missing)
          (cons :redundant-file-missing redundant-file-missing))))



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
;;
;; Keywords: database persistent hash weed


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
    (setq picp-db (cond ((and (hash-table-p db) (null old))
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
    ;; PENDING - optionally sort list by tags
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




;;; Idle timer functions.

(defvar picp-timers nil)
(defvar picp-idle-timer-work-functions
  '((picp-maybe-save-journal 0.2)
    (picp-look-ahead-next 0.2)
    ;; PENDING
    ;; (picp-look-ahead-more 2)
    (picp-compute-filter-index 0.5)
    (picp-compute-filter-match-count 1)
    (picp-travserse-pic-list 3)
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
  (let ((debug-on-error t)
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


(defun picp-travserse-pic-list (deadline-function state)
  (with-current-buffer picp-buffer
    (cl-loop for pic on (picp-continue-or-start-over state)
             do (progn
                  (picp-sha-via-cache pic)
                  ;; PENDING - may push out look ahead from cache?
                  (picp-size-via-cache pic))
             until (funcall deadline-function pic))))

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
               when (eq (picp-pos-current pos) picp-current)
               return (setq picp-filter-index (picp-pos-filter-index pos))
               until (funcall deadline-function pos)))))



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


;;; Buffer functions.

(defun picp-update-buffer ()
  (let ((s (cadr (picp-time (picp-do-update-buffer)))))
    (picp-debug s "%s %s" this-command picp-index)))

(defun picp-do-update-buffer ()
  (unless (equal (buffer-name) picp-buffer)
    (error "This requires picpocket mode"))
  (if (picp-try-ensure-matching-picture)
      (progn
        (picp-insert picp-current)
        (cd (picp-dir)))
    (picp-no-pictures))
  (force-mode-line-update))

(defun picp-try-ensure-matching-picture ()
  "Return nil if no matching picture was found."
  (when picp-current
    (or (picp-filter-match-p picp-current)
        (picp-when-let (pos (or (picp-next-pos) (picp-previous-pos)))
          (picp-set-pos pos)
          t))))

(defun picp-no-pictures ()
  (let (buffer-read-only)
    (erase-buffer)
    (insert (propertize (format "\n\nNo pictures in list%s.\n\n"
                                (if picp-filter
                                    (format " matching filter %s" picp-filter)
                                  ""))
                        'face 'bold))
    (when picp-filter
      (insert (format "Type %s to edit filter.\n"
                      (picp-where-is 'picp-filter))))
    (and (eq picp-entry-function 'picpocket-dir)
         (not picp-recursive)
         (insert
          (format "Type %s to recursively include pictures in subdirectories.\n"
                  (picp-where-is 'picp-toggle-recursive))))
    (insert (format "Type %s for dired in %s.\n"
                    (picp-where-is 'picp-dired)
                    (abbreviate-file-name default-directory)))))

(defun picp-where-is (command)
  (let ((binding (where-is-internal command overriding-local-map t)))
    (propertize (if binding
                    (key-description binding)
                  (concat "M-x " (symbol-name command)))
                'face 'bold)))



(defun picp-create-buffer (files &optional selected-file dir)
  ;; (unless (picp-imagemagick-p)
  ;; (error "Picpocket requires Emacs compiled with imagemagick support"))
  (setq selected-file (and selected-file
                           (file-truename selected-file)))
  (let ((old-buffer (get-buffer picp-buffer)))
    (when old-buffer
      (kill-buffer old-buffer)))
  (with-current-buffer (get-buffer-create picp-buffer)
    (picp-mode)
    (when dir
      (cd dir))
    (condition-case err
        (picp-create-picp-list files selected-file)
      (quit (picp-reset-list)
            (signal (car err) (cdr err))))
    (picp-update-buffer)
    (if (called-interactively-p 'any)
        (switch-to-buffer (current-buffer))
      (set-buffer (current-buffer)))
    (current-buffer)))


;;; Image handling.

(defun picp-insert (pic)
  (let (buffer-read-only)
    (erase-buffer)
    (when pic
      (if (display-images-p)
          (insert-image (picp-create-image pic (picp-save-window-size)))
        (insert "\n\nThis display does not support images.")))
    (goto-char (point-min))))

(defun picp-save-window-size ()
  "Save the current window size.
This is for the benefit of timer functions that do not
necessarily run with the picpocket window selected."
  (cl-destructuring-bind (x0 y0 x1 y1) (window-inside-pixel-edges)
    ;; PENDING - For some reason Emacs 25.0 refuses to draw image in a
    ;; right margin that seem to be (frame-char-width) pixels wide.
    ;; Therefore subtract that.
    (setq picp-window-size (cons (- x1 x0 (frame-char-width))
                                 (- y1 y0)))))

(defun picp-create-image (pic canvas-size)
  (pcase-let ((`(,keyword . ,value) (picp-clock
                                     (picp-size-param pic canvas-size))))
    (create-image (picp-path pic)
                  ;; PENDING - Seem like imagemagick svg support is
                  ;; somewhat broken.
                  (unless (string-suffix-p ".svg" (picp-file pic) t)
                    'imagemagick)
                  nil
                  :rotation (picp-rotation pic)
                  keyword (picp-scale value))))

(defun picp-size-param (pic canvas-size)
  (pcase-let ((canvas-ratio (picp-cons-ratio canvas-size))
              (rot-ratio (picp-cons-ratio (picp-rotated-size pic)))
              (`(,pic-x . ,_) (picp-size-via-cache pic)))
    (pcase picp-fit
      (:x-and-y (if (> canvas-ratio rot-ratio)
                    (picp-height-size-param pic canvas-size)
                  (picp-width-size-param pic canvas-size)))
      (:x (picp-width-size-param pic canvas-size))
      (:y (picp-height-size-param pic canvas-size))
      (_ (cons :width pic-x)))))

(defun picp-height-size-param (pic canvas-size)
  (pcase-let* ((`(,_ . ,canvas-y) canvas-size)
               (`(,_ . ,pic-y) (picp-size-via-cache pic))
               (`(,_ . ,rot-y) (picp-rotated-size pic))
               (y-ratio (picp-ratio pic-y rot-y)))
    (cons :height (round (* y-ratio canvas-y)))))

(defun picp-width-size-param (pic canvas-size)
  (pcase-let* ((`(,canvas-x . ,_) canvas-size)
               (`(,pic-x . ,_) (picp-size-via-cache pic))
               (`(,rot-x . ,_) (picp-rotated-size pic))
               (x-ratio (picp-ratio pic-x rot-x)))
    (cons :width (round (* x-ratio canvas-x)))))

(defun picp-size-via-cache (pic)
  (or (picp-size pic)
      (picp-save-size-in-pic pic)))

(defun picp-save-size-in-pic (pic)
  (picp-set-size pic (image-size (create-image (picp-path pic)
                                               nil nil
                                               :rotation 0)
                                 t)))

(defun picp-rotated-size (pic)
  (if (zerop (picp-rotation pic))
      (picp-size-via-cache pic)
    (image-size (create-image (picp-path pic)
                              nil nil
                              :rotation (picp-rotation pic))
                t)))

(cl-defun picp-cons-ratio ((a . b))
  (/ (float a) b))

(cl-defun picp-ratio (a b)
  (/ (float a) b))

(defun picp-look-ahead-and-save-time (pic)
  (let ((s (cadr (picp-time (picp-look-ahead pic))))
        (picp-sum 0))
    (picp-debug s "look")))

(defun picp-look-ahead (pic)
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
          (t (concat (if remove "-" "")
                     (picp-lookup-key-strict key))))))

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
  (cl-loop for (key action arg) in (if (symbolp picp-keystroke-alist)
                                       (symbol-value picp-keystroke-alist)
                                     picp-keystroke-alist)
           do (define-key picp-mode-map
                (picp-key-vector key)
                (cond ((eq action 'tag)
                       (picp-tag-command arg))
                      ((memq action '(move copy hardlink))
                       (picp-other-command action arg))
                      ((and (symbolp action)
                            (fboundp action))
                       action)
                      (t
                       (error "Invalid entry in picp-keystroke-alist (%s %s %s)"
                              key action arg)))))
  (when (buffer-live-p (get-buffer picp-buffer))
    (with-current-buffer picp-buffer
      (use-local-map picp-mode-map))))

(defun picp-tag-command (tag)
  "Create a command that add TAG to current picture."
  (let ((symbol (intern (picp-command-name 'tag tag))))
    (fset symbol `(lambda ()
                    (interactive)
                    (picp-action 'tag ,tag)
                    (picp-update-buffer)))
    (put symbol 'picp-user-command 'tag)
    symbol))

(defun picp-other-command (action dst)
  "Create a command that move/copy/hardlink the current picture.
ACTION is one of the symbols move, copy or hardlink.
DST is the destination directory."
  (let ((symbol (intern (picp-command-name action dst))))
    (fset symbol `(lambda ()
                    (interactive)
                    (picp-action ',action ,dst t)
                    ,(when (eq action 'move)
                       '(picp-update-buffer))))
    (put symbol 'picp-user-command 'other)
    symbol))

(defun picp-command-name (action arg)
  (pcase action
    (`tag (concat "picp-add-tag-" arg))
    (`move (concat "picp-move-to-" arg))
    (`copy (concat "picp-copy-to-" arg))
    (`hardlink (concat "picp-hardlink-to-" arg))
    (f (symbol-name f))))


;;; Help functions

(defvar picp-help-count 0)
(defvar picp-is-sole-window nil)

(defvar picp-help-map
  (let ((map (make-sparse-keymap)))
    (define-key map [??] 'picp-help)
    map))

(defun picp-help ()
  "Toggle display of available commands.

First invocation will display user defined commands in a help
window if there is any.  User defined commands are defined by
setting the variable `picp-keystroke-alist').

Second invocation will display built-in commands for picpocket
mode.

Third invocation will hide the help."
  (interactive)
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
         (picp-hide-help)))))

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
           when (string-equal buffer-name (buffer-name (window-buffer window)))
           return window))

(defun picp-visible-buffers ()
  (mapcar (lambda (window)
            (buffer-name (window-buffer window)))
          (window-list)))


(defun picp-shrink-to-fit ()
  (when (window-combined-p nil t)
    (shrink-window-horizontally (- (window-width) (picp-buffer-width)))))

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
             (commands (sort (picp-keymap-to-list nil picp-mode-map predicate)
                             (lambda (a b)
                               (string-lessp (picp-key-sort-string (car a))
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
  (princ (format " (%s %s%s)"
                 picp-last-action
                 picp-last-arg
                 (if picp-last-arg-is-dir "/" ""))))


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


;;; File managemet and tag command help functions.

(defun picp-action (action arg &optional dst-is-dir pic)
  (if (eq action 'tag)
      (progn
        (picp-add-tag arg pic)
        (message "%s is tagged with %s." (picp-file pic) arg)
        (force-mode-line-update)
        (picp-save-last-action action arg dst-is-dir))
    (let ((old-dir (picp-dir pic))
          (new-path (picp-new-path-for-file-action arg dst-is-dir pic)))
      (picp-file-action action new-path pic)
      (unless (string-equal (file-name-nondirectory new-path) old-dir)
        (picp-save-last-action action arg dst-is-dir)))))

(defun picp-save-last-action (action arg arg-is-dir)
  (setq picp-last-action action
        picp-last-arg arg
        picp-last-arg-is-dir arg-is-dir))

(defun picp-new-path-for-file-action (dst dst-is-dir pic)
  (unless (or (file-name-absolute-p dst)
              picp-dst-dir-is-cwd)
    (setq dst (concat picp-dst-dir dst)))
  (file-truename (if (or dst-is-dir
                         (file-directory-p dst))
                     (concat (file-name-as-directory dst)
                             (picp-file pic))
                   dst)))


(defun picp-file-action (action new-path pic)
  (let* ((pic (or pic picp-current))
         (old-dir (picp-dir pic))
         (old-file (picp-file pic))
         (old-path (concat old-dir old-file))
         (new-dir (file-name-directory new-path))
         (new-file (file-name-nondirectory new-path))
         ok-if-already-exists
         identical)
    (make-directory new-dir t)
    (while (and (file-exists-p new-path)
                (not identical))
      (cond ((equal old-path new-path)
             (user-error "Attempt to %s file to itself"
                         (picp-action-string action)))
            ((picp-files-identical-p old-path new-path)
             (setq ok-if-already-exists
                   (y-or-n-p (concat "Identical file already exists in "
                                     new-dir
                                     "  Overwrite? "))
                   identical t))
            ((file-directory-p new-path)
             (setq new-file
                   (read-string
                    (format "Directory %s already exists.  Rename this to: "
                            new-path)
                    old-file))
             (setq new-path (concat new-dir new-file)))
            (t
             (unwind-protect
                 (let (picp-adapt-to-window-size-change)
                   (picp-compare pic new-path)
                   (setq new-file
                         (read-string
                          (format (concat "File already exists (size %s)."
                                          "  Rename this (size %s) to: ")
                                  (picp-file-kb new-path)
                                  (picp-file-kb old-path))
                          old-file))
                   (setq new-path (concat new-dir new-file)))
               (picp-update-buffer)))))
    (cl-case action
      (move
       (picp--move old-path new-path ok-if-already-exists pic))
      ((copy hardlink)
       (picp--duplicate action old-path new-path ok-if-already-exists))
      (t (error "Invalid picpocket action %s" action)))))

(defun picp-action-string (action)
  (cl-case action
    (copy "copy")
    (move "rename")
    (hardlink "hard link")))

(defun picp--move (old-path new-path ok-if-already-exists pic)
  (let ((new-dir (file-name-directory new-path))
        (new-file (file-name-nondirectory new-path))
        (old-dir (file-name-directory old-path))
        (old-file (file-name-nondirectory old-path))
        (inhibit-quit t))
    (rename-file old-path new-path ok-if-already-exists)
    (picp-tags-move-file pic old-path new-path)
    (if (equal old-dir new-dir)
        (progn
          (picp-set-file pic new-file)
          (message "Renamed %s to %s." old-file new-file))
      (picp-delete-pic pic)
      (message "Moved %s to %s." old-file new-dir))))

(defun picp--duplicate (action old-path new-path ok-if-already-exists)
  (let ((old-file (file-name-nondirectory old-path))
        (inhibit-quit t))
    (picp-tags-copy-file picp-current new-path)
    (if (eq action 'copy)
        (copy-file old-path new-path ok-if-already-exists)
      (add-name-to-file old-path new-path ok-if-already-exists))
    (picp-duplicate-message action old-file new-path)))

(defun picp-duplicate-message (action old dst)
  (message "%s %s to %s."
           (if (eq action 'copy)
               "Copied"
             "Hard linked")
           old
           dst))


(defun picp-compare (pic new)
  (cl-destructuring-bind (window-width . window-height) (picp-save-window-size)
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
                      (picp-file-kb new)))
      (insert-image (picp-create-image (picp-make-pic new)
                                       (cons window-width pic-height)))
      (insert (format "\nWith this picture (%s):\n" (picp-pic-kb pic)))
      (insert-image (picp-create-image pic (cons window-width pic-height)))
      (goto-char (point-min)))))

(defun picp-standard-value (symbol)
  (eval (car (get symbol 'standard-value))))



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

;; (add-hook 'echo-area-clear-hook #'jcl-clear-echo)
;; (defun jcl-clear-echo ()
;; (picp-msg "clear echo"))


(defun picp-header-pic-info ()
  (when picp-current
    (picp-join (concat (format "%s/%s " picp-index picp-length)
                       (picp-escape-percent (picp-header-dir))
                       "/"
                       (propertize (picp-escape-percent (picp-file))
                                   'face 'highlight))
               (when picp-debug
                 picp-header-text)
               (picp-pic-kb picp-current)
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

(defun picp-pic-kb (pic)
  (picp-kb (picp-bytes pic)))

(defun picp-file-kb (file)
  (picp-kb (picp-file-bytes file)))

(defun picp-file-bytes (file)
  (unless (file-exists-p file)
    (error "File %s do not exist" file))
  (elt (file-attributes file) 7))

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
  (when tags
    (cl-case picp-tags-style
      (org (format ":%s:" (mapconcat #'symbol-name tags ":")))
      (t (format "(%s)" (mapconcat #'symbol-name tags " "))))))

(defun picp-filter-info ()
  (when picp-filter
    (format "(filter: %s %s/%s)"
            (mapconcat 'symbol-name picp-filter " ")
            (or picp-filter-index "?")
            (cond (picp-filter-match-count-done
                   picp-filter-match-count)
                  ((null picp-filter-match-count)
                   "?")
                  ((zerop picp-filter-match-count)
                   "?")
                  (t
                   (format "%s+" picp-filter-match-count))))))


;;; Misc help functions

(defvar picp-done-dirs)
(defvar picp-file-count)

(defun picp-file-list (dir)
  (let ((picp-file-count 0)
        (picp-done-dirs nil))
    (prog1
        (picp-file-list2 (directory-file-name (file-truename dir)))
      (message "Found %s pictures" picp-file-count))))

(defun picp-file-list2 (dir)
  (push dir picp-done-dirs)
  (condition-case err
      (let ((files (directory-files dir t "[^.]"))
            pic-files sub-files subdirs)
        (dolist (file files)
          (if (file-directory-p file)
              (push file subdirs)
            (when (string-match (picp-picture-regexp) file)
              (push file pic-files)
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
              (let ((true-subdir (directory-file-name (file-truename subdir))))
                (unless (or (picp-dot-file-p subdir)
                            (member true-subdir picp-done-dirs))
                  (setq sub-files (append (picp-file-list2 true-subdir)
                                          sub-files)))))))
        (append pic-files sub-files))
    (file-error (progn
                  (warn "Failed to access %s (%s)" dir err)
                  nil))))

(defun picp-dot-file-p (file)
  "Return t if the FILE's name start with a dot."
  (eq (elt (file-name-nondirectory file) 0) ?.))

(defun picp-y-or-n-p (format &rest objects)
  (let* ((prompt (apply #'format format objects))
         (header-line-format (concat prompt " (y or n)")))
    (y-or-n-p prompt)))


(defun picp-window-size-change-function (frame)
  (when picp-adapt-to-window-size-change
    (dolist (window (window-list frame 'no-minibuffer))
      (when (eq (get-buffer picp-buffer) (window-buffer window))
        (with-selected-window window
          (with-current-buffer picp-buffer
            (unless (equal picp-window-size (picp-save-window-size))
              (picp-update-buffer))))))))


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

(defun picp-path (&optional pic)
  (unless pic
    (setq pic picp-current))
  (concat (picp-dir pic) (picp-file pic)))


(defun picp-warn (format &rest args)
  (if picp-demote-warnings
      (apply #'message (concat "picpocket-warning: " format) args)
    (apply #'warn format args)))

(defun picp-dst-dir ()
  (if picp-dst-dir-is-cwd
      default-directory
    picp-dst-dir))

(defun picp-mapc (f)
  (cl-loop for pic on picp-list
           when (picp-filter-match-p pic)
           do (funcall f pic)))

(defun picp-mapcar (f)
  (cl-loop for pic on picp-list
           when (picp-filter-match-p pic)
           collect (funcall f pic)))

(defun picp-debug (s format &rest args)
  (when picp-debug
    (setq picp-sum (time-add picp-sum s))
    (setq picp-header-text (format "(%s %s (%s))"
                                   (apply #'format format args)
                                   (picp-sec-string s)
                                   (picp-sec-string picp-sum)))
    (message "picp-debug: %s" picp-header-text)))

(defun picp-clear-image-cache ()
  "Clear image cache.  Only useful for benchmarks."
  (interactive)
  (setq picp-sum 0)
  (message "Clear image cache %s" (picp-time-string (clear-image-cache t))))


(defun picp-dump ()
  "Print some picpocket variables."
  (interactive)
  (picp-print 'picp-list)
  (picp-print 'picp-current)
  (picp-print 'picp-index)
  (picp-print 'picp-length)
  (message "")
  (view-echo-area-messages)
  t)

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

(provide 'picpocket)

;;; picpocket.el ends here
