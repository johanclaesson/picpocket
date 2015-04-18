;;; picpocket.el --- Image viewer -*- lexical-binding: t; coding: utf-8-unix-*-

;; Copyright (C) 2015 Johan Claesson
;; Author: Johan Claesson <johanclaesson@bredband.net>
;; Maintainer: Johan Claesson <johanclaesson@bredband.net>
;; Created: 2015-02-16
;; Time-stamp: <2015-04-18 14:05:51 jcl>
;; Version: 10

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

;; Picpocket is an image viewer which requires GNU Emacs 24 compiled
;; with ImageMagick.  It has commands for:
;;
;; * File operations on the image files (delete, move, copy, hardlink).
;; * Associate images with tags which are saved to disk.
;; * Customizing keystrokes for tagging and file operations.
;; * A simple slide show mode.
;;
;; Main entry point
;; ----------------
;;
;; Command: picpocket
;;  View the images in the current directory.
;;
;; Main keybindings
;; ----------------
;;
;; Space     - Next image.
;; BackSpace - Previous image.
;; r         - Rename image file.
;; c         - Copy image file.
;; k         - Delete image file.
;; t         - Edit tags.
;; s         - Slide-show mode.
;; [         - Rotate counter-clockwise.
;; ]         - Rotate clockwise.
;; +         - Scale in.
;; -         - Scale out.
;; e         - Customize keystrokes (see below).
;; TAB f     - Toggle full-screen.
;; TAB r     - Toggle recursive inclusion of images in sub-directories.
;;
;; With prefix argument many of the commands will operatate on all the
;; pictures in the current list instead of just the current picture.
;;
;; Disclaimer
;; ----------
;;
;; Picpocket will secretly do stuff in the background with idle
;; timers.  This includes to load upcoming images into the image
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
;; Keystokes can be bound to commands that move/copy images into
;; directories and/or tag them.  The following creates commands on
;; keys 1 though 5 to tag according to liking.  Also creates some
;; commands to move image files to directories according to genre.
;; Finally creates also one command to copy images to a backup
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
;; Tags associated with images are saved to disk.  By default to
;; ~/.emacs.d/picpocket/.  This database maps the sha1 checksum of
;; image files to list of tags.  This implies that you can rename or
;; move around the file anywhere and the tags will still be remembered
;; when you view it with picpocket again.
;;
;; If you change the image file content the sha1 checksum will change.
;; For example this would happen if you rotate or crop the image with
;; an external program.  That will break the association between sha1
;; checksum and tags.  However picpocket also stores the file name for
;; each entry of tags.  The command `picp-db-update' will go through
;; the database and offer to recover such lost associations.
;;
;; If you change the file-name and the file content at the same time
;; there is no way to recover automatically.



;;; Code:

;; TODO.
;; * Remove all &optional before pic.
;; * defvar -> defcustom where appropriate.
;; * Activate picp-look-ahead-more
;; ** Maybe implement resume timers for it
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

;; NICE
;; * Smoother movement commands (when image is bigger than window)
;; * The tags for a filename is it's directory name plus the picp-tags.
;; * Underscores and dashes should be ignored in tag names.
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
;; * Some values of LC_COLLATE and LANG makes ls sorting case
;;   insensitive.  directory-files always sorts by as cii.  Could do a
;;   one-time test by calling ls in a tmp directory with files A and
;;   b.
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


(eval-when-compile
  (require 'time-date)
  (require 'ido))

(require 'cl-lib)
(require 'dired)



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


(defconst picp-buffer "*picpocket*")
(defvar picp-frame nil)
(defvar picp-old-frame nil)
(defvar picp-header t)
(defvar picp-dst-dir "~/")
(defvar picp-dst-dir-is-cwd t)
(defvar picp-recursive nil)
(defvar picp-last-action nil)
(defvar picp-last-arg nil)
(defvar picp-last-arg-is-dir nil)

(defvar picp-header-line-format '(:eval (picp-header-line)))
(defvar picp-header-full-path nil)
(defvar picp-dired-when-no-images-p nil)
(defvar picp-look-ahead-max 5)
(defvar picp-ask-before-delete t)



(defvar picp-look-count 0)

(defvar picp-tags-style 'list)
(defvar picp-demote-warnings nil)
(defvar picp-debug nil)
(defvar picp-image-regexp nil)
(defvar picp-backdrop-command nil)
(defvar picp-default-backdrop-commands
  '(("display" . "-window root")
    ("feh" . "--bg-file")
    ("hsetroot" . "-tile")
    ("xsetbg")))



;;
;; Buffer local variables.
;;
;; Currently most variables are global and there can only be one
;; picpocket buffer at a time.
(defvar picp-entry-function nil)
(defvar picp-entry-args nil)
(defvar picp-filter nil)
(defvar picp-index nil
  "The `picp-index' is starting from 1 (incompatible with elt).")
(defvar picp-length nil)
(defvar picp-window-size nil
  "The current window size in pixels.
This is kept for the benefit of timer functions that do not
necessarily run with the picpocket window selected.")
(defvar picp-header-text "")
(defvar picp-list nil
  "The `picp-list' is a double-linked list of all images in directory.
The car contains a `picp-pic' struct whose prev slot points to
the previous cons cell.  The next cell is in the cdr.

Note that this is a circular data structure and `print-circle'
need to be non-zero when printing it.  If it is nil Emacs will
hang.")
(defvar picp-current nil)

(put 'picp-index 'risky-local-variable t)
(put 'picp-length 'risky-local-variable t)
(put 'picp-header-text 'risky-local-variable t)
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

(defmacro picp-time-string (&rest forms)
  (declare (indent defun))
  `(cadr (picp-measure-time ,@forms)))

(defmacro picp-measure-time (&rest forms)
  "Evaluate FORMS and return (rc time-string)."
  (declare (indent defun))
  (let ((begin-time (make-symbol "begin-time"))
        (rc (make-symbol "rc")))
    `(let ((,begin-time (current-time))
           (,rc (progn ,@forms)))
       (with-decoded-time-value
           ((sec-hi sec-low micro (time-subtract (current-time) ,begin-time)))
         (list ,rc (format "%d.%03d_%03ds"
                           (+ (lsh sec-hi 16) sec-low) (/ micro 1000) (% micro 1000)))))))


;;; Picp mode

(define-derived-mode picp-mode special-mode "picpocket"
  (buffer-disable-undo)
  (make-local-variable 'picp-index)
  (make-local-variable 'picp-header-text)
  (make-local-variable 'picp-list)
  (make-local-variable 'picp-current)
  (make-local-variable 'picp-length)
  (setq-local image-type-file-name-regexps
              (list (rassq 'imagemagick image-type-file-name-regexps)))
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
                           " [D - debug]"))))
  (define-key toggle-map [?f] #'picp-toggle-fullscreen-frame)
  (define-key toggle-map [?d] #'picp-toggle-dst-dir-is-cwd)
  (define-key toggle-map [?h] #'picp-toggle-header)
  (define-key toggle-map [?r] #'picp-toggle-recursive)
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
  (define-key map [?l] #'picp-hard-link)
  (define-key map [(meta ?m)] #'picp-move-by-keystroke)
  (define-key map [(meta ?c)] #'picp-copy-by-keystroke)
  (define-key map [(meta ?l)] #'picp-hard-link-by-keystroke)
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
  (define-key map [?+] #'picp-scale-in)
  (define-key map [?=] #'picp-scale-in)
  (define-key map [?-] #'picp-scale-out)
  (define-key map [?0] #'picp-no-scale)
  (define-key map [?b] #'picp-fit-to-width-and-height)
  (define-key map [?w] #'picp-fit-to-width)
  (define-key map [?h] #'picp-fit-to-height)
  (define-key map [?n] #'picp-no-fit)
  (define-key map [??] #'picp-help)
  (define-key map [left] #'scroll-right)
  (define-key map [right] #'scroll-left)
  (define-key map [(control ?b)] #'scroll-right)
  (define-key map [(control ?f)] #'scroll-left)
  (define-key map [??] #'picp-help)
  ;; PENDING
  ;; (define-key map [?j] #'picp-jump)
  (setq picp-mode-map map))

;;; Entry points.

;;;###autoload
(defun picpocket ()
  "View the images in the current directory."
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
  (let ((files (picp-file-list dir picp-recursive)))
    (picp-create-buffer files selected-file dir)))

(defun picpocket-files (files &optional selected-file)
  (setq picp-entry-function 'picpocket-files
        picp-entry-args (list files))
  (picp-create-buffer files selected-file))


;;; Picpocket mode commands.

(defun picp-rotate-counter-clockwise (&optional arg)
  "Display the current picture rotated 90 degrees to the left.
This command do not change the image file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picp-rotate -90.0 arg))

(defun picp-rotate-clockwise (&optional arg)
  "Display the current picture rotated 90 degrees to the right.
This command do not change the image file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picp-rotate 90.0 arg)
  (picp-update-buffer))

(defun picp-rotate (delta arg)
  (let ((degrees (if arg
                     (float (read-number "Set rotation in degrees"
                                         (picp-rotation)))
                   (+ (picp-rotation) delta))))
    (picp-set-rotation picp-current degrees)
    (picp-update-buffer)))


(defvar picp-fit :width-and-height)
(defvar picp-scale 100)

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

(defun picp-no-scale ()
  "Reset the scaling to 100%."
  (interactive)
  (setq picp-scale 100)
  (message "No scaling.")
  (picp-update-buffer))

(defun picp-fit-to-width-and-height ()
  "Fit the picture to both width and height of window.
Fitting is done before applying the scaling factor.  That is, it
will fit only when the scaling is the default 100%.  The scaling
can be restored to 100% by typing \\[picp-no-scale] \(for
`picp-no-scale')."
  (interactive)
  (setq picp-fit :width-and-height)
  (message "Fit picture to both width and height")
  (picp-update-buffer))

(defun picp-fit-to-width ()
  "Fit the picture to the width of window.
Fitting is done before applying the scaling factor.  That is, it
will fit only when the scaling is the default 100%.  The scaling
can be restored to 100% by typing \\[picp-no-scale] \(for
`picp-no-scale')."
  (interactive)
  (setq picp-fit :width)
  (message "Fit picture to width")
  (picp-update-buffer))

(defun picp-fit-to-height ()
  "Fit the picture to the height of window.
Fitting is done before applying the scaling factor.  That is, it
will fit only when the scaling is the default 100%.  The scaling
can be restored to 100% by typing \\[picp-no-scale] \(for
`picp-no-scale')."
  (interactive)
  (setq picp-fit :height)
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
  "Toggle recursive inclusion of sub-directories."
  (interactive)
  (setq picp-recursive (not picp-recursive))
  (picp-revert)
  (message (if picp-recursive
               "Recursively include images in subdirectories."
             "Only show images in current directory.")))

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
  (when (and picp-frame
             (frame-live-p picp-frame))
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
    (user-error "You need to set picp-keystroke-alist for this command to work"))
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
frame in fullscreen mode.  The frame created will have no
scrollbars or fringes.  It is meant to only show the picpocket
buffer (but this is not enforced).  The second call will delete
this frame and go back to the old frame."
  (interactive)
  (if (and picp-frame
           (frame-live-p picp-frame))
      (progn
        (delete-frame picp-frame)
        (setq picp-frame nil)
        (picp-select-frame picp-old-frame)
        (setq mode-line-format (default-value 'mode-line-format))
        (picp-update-buffer))
    (setq picp-old-frame (selected-frame)
          picp-frame (make-frame '((name . "picpocket")
                                      ;; (minibuffer . nil)
                                      ;; (cursor-type . nil)
                                      (fullscreen . fullboth)
                                      ;; PENDING - background-color messing up the cache? YES
                                      ;; See image.c:search_image_cache.
                                      ;; (foreground-color . "white")
                                      ;; (background-color
                                      ;; . "black"))))
                                      )))
    ;; Scroll bars and fringe are better handled on buffer level.
    ;; (vertical-scroll-bars . nil)
    ;; (left-fringe . 0)
    ;; (right-fringe . 0)
    (picp-select-frame picp-frame)
    (setq mode-line-format nil)
    ;; Resdisplay seem to be needed to get accurate return value from
    ;; window-inside-pixel-edges.
    (redisplay)
    (picp-update-buffer)))

(defun picp-select-frame (frame)
  (select-frame-set-input-focus frame)
  (switch-to-buffer picp-buffer)
  ;; Update fringes.
  (set-window-buffer (selected-window) (current-buffer)))


(defun picp-next ()
  "Move to the next picture in the current list."
  (interactive)
  (let ((next (picp-next-pic)))
    (if next
        (let ((inhibit-quit t))
          (setq picp-current next
                picp-index (picp-calculate-index)))
      (picp-no-file "next")))
  (picp-update-buffer))

(defun picp-next-pic ()
  (cl-loop for pic = (cdr picp-current) then (cdr pic)
           unless pic return nil
           when (picp-filter-match-p pic) return pic))

(defun picp-previous ()
  "Move to the previous picture in the current list."
  (interactive)
  (unless (picp-set-current (picp-previous-pic))
    (picp-no-file "previous"))
  (picp-update-buffer))

(defun picp-previous-pic ()
  (cl-loop for pic = (picp-safe-prev picp-current) then (picp-safe-prev pic)
           unless pic return nil
           when (picp-filter-match-p pic) return pic))

(defun picp-safe-prev (pic)
  (and pic
       (picp-prev pic)))

(defun picp-set-current (pic)
  (when pic
    (let ((inhibit-quit t))
      (setq picp-current pic
            picp-index (picp-calculate-index)))))


(defun picp-home ()
  "Move to the first picture in the current list."
  (interactive)
  (let ((inhibit-quit t))
    (setq picp-current picp-list
          picp-index 1))
  (picp-update-buffer))

(defun picp-end ()
  "Move to the last picture in the current list."
  (interactive)
  (let ((inhibit-quit t))
    (while (cdr picp-current)
      (setq picp-current (cdr picp-current)
            picp-index (1+ picp-index))))
  (picp-update-buffer))


(defun picp-delete-file ()
  "Permanently delete the current picture file."
  (interactive)
  (let ((file (picp-file)))
    (when (if picp-ask-before-delete
              (picp-y-or-n-p "Delete %s? " file)
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

(defun picp-hard-link (all)
  "Make a hard link to the current picture in another directory.
With prefix arg (ALL) hard link all pictures in the current list."
  (interactive "P")
  (if all
      (picp-hard-link-all (read-directory-name "Hard link all pictures to: "
                                               (picp-dst-dir)))
    (picp-action 'hard-link (read-directory-name "Hard link to: " (picp-dst-dir)))))

(defun picp-hard-link-all (dst)
  (picp-mapc (lambda (pic)
               (picp-action 'hard-link dst t pic)))
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

(defun picp-hard-link-by-keystroke (all)
  "Make a hard link to the current picture.
The destination directory is determined by a keystroke that is
lookup up in the variable `picp-keystroke-alist'.
With prefix arg (ALL) hard link all pictures in the current list."
  (interactive "P")
  (if all
      (let ((dst (picp-read-key "directory to link all pictures to")))
        (picp-mapc (lambda (pic)
                     (picp-action 'hard-link dst t pic))))
    (picp-action 'hard-link (picp-read-key "directory to link to"))))


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
    (picp-action 'tag (picp-read-key-to-add-or-remove-tag))))

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
    (let* ((old-tags-string (mapconcat #'symbol-name (picp-tags picp-current) " "))
           (new-tags-string (read-string "Tags: " old-tags-string))
           (new-tag-symbols (mapcar #'intern (split-string new-tags-string))))
      (picp-tags-set picp-current new-tag-symbols)
      (message "Tags for %s is %s."
               (picp-file)
               (if new-tag-symbols
                   new-tag-symbols
                 "cleared")))))

;; (defun picp-tag-all-by-string ()
;; "Add a tag to all pictures in the current list.
;; Type a minus before the tag to remove instead of add."
;; (interactive)
;; (picp-tag-to-all (read-string "Type tag to add to all files (-tag to remove): ")))

;; (defun picp-tag-all-by-keystroke ()
;; "Add a tag to all pictures in the current list.
;; The tag is determined by a keystroke that is looked up in the
;; variable `picp-keystroke-alist'."
;; (interactive)
;; (picp-tag-to-all (picp-read-key-to-add-or-remove-tag nil t)))


(defun picp-set-filter (filter-string)
  "Enter the current picpocket filter.
The filter is a list of tags.  Only pictures with all the tags in
the filter is shown.  To enter multiple tags separate them with
spaces.

When called from Lisp the argument FILTER-STRING is a
space-separated string."
  (interactive (list (read-string "Show only pictures with this tag: ")))
  (setq picp-filter (mapcar #'intern (split-string filter-string)))
  (picp-update-buffer))

(defun picp-filter-match-p (pic)
  (cl-subsetp picp-filter (picp-tags pic)))

(defun picp-no-file (&optional text)
  (when text
    (setq text (concat " " text)))
  (if picp-filter
      (user-error "No%s file match filter %s" text picp-filter)
    (user-error "No%s file" text)))

(defun picp-set-filter-by-keystroke ()
  "Show only pictures having the tag in the current filter."
  (interactive)
  (picp-set-filter (picp-read-key "filtering tag")))

;;; Pic double-linked list functions.

;; These will call tag handling functions.

(defun picp-make-pic (path)
  (list (make-picp-pic :dir (file-name-directory path)
                       :file (file-name-nondirectory path))))

(defun picp-reset ()
  (setq picp-list nil
        picp-current nil
        picp-index 0
        picp-length 0
        picp-filter nil))

(defun picp-delete ()
  (let ((file (picp-path)))
    (let ((inhibit-quit t))
      (delete-file file)
      (picp-tags-delete-file picp-current file)
      (picp-delete-pic picp-current))))

(defun picp-delete-pic (&optional pic)
  (setq pic (or pic
                picp-current))
  (clear-image-cache (picp-path pic))
  (setq picp-length (1- picp-length))
  (if (picp-prev pic)
      (setcdr (picp-prev pic) (cdr pic))
    (setq picp-list (cdr pic)))
  (if (cdr pic)
      (progn
        (picp-set-prev (cdr pic) (picp-prev pic))
        (when (eq picp-current pic)
          (setq picp-current (cdr pic))))
    (if (picp-prev pic)
        (when (eq picp-current pic)
          (setq picp-current (picp-prev pic)))
      (picp-reset)))
  (setq picp-index (picp-calculate-index)))

(defun picp-create-picp-list (files &optional selected-file)
  (picp-reset)
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
               (string-match (picp-image-regexp) selected-file)
               (cl-loop for pic on picp-list
                        for index = 1 then (1+ index)
                        when (equal selected-file
                                    (concat (picp-dir pic) (picp-file pic)))
                        return (setq picp-index index
                                     picp-current pic)))
    (setq picp-index 1
          picp-current picp-list)))




;;; Tag database interface functions

;; This layer translates from struct picp-pic to a sha1 checksum.
;; This checksum is refered to as sha and is used as index in the
;; database below.

(defun picp-tags (&optional pic)
  (picp-db-tags (picp-safe-sha pic)))

(defun picp-tags-set (pic new-tags)
  (picp-db-tags-set (picp-safe-sha pic)
                    (picp-path pic)
                    new-tags))

(defun picp-tags-move-file (pic old-file new-file)
  (picp-db-tags-move-file (picp-safe-sha pic) old-file new-file))

(defun picp-tags-copy-file (pic new-file)
  (picp-db-tags-copy-file (picp-safe-sha pic) new-file))

(defun picp-tags-delete-file (pic deleted-file)
  (picp-db-tags-delete-file (picp-safe-sha pic) deleted-file))

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



(defun picp-safe-sha (&optional pic)
  "Return the sha1 checksum of PIC.
The checksum will be computed if not already available.
Also, if there is a matching entry in the database with tags
then the file of PIC will be added to that entry."
  (let ((sha (picp-sha pic))
        (file (picp-path pic)))
    (unless sha
      (setq sha (picp-sha1sum file))
      (let ((inhibit-quit t))
        (picp-set-sha pic sha)
        (picp-db-tags-add-file sha file)))
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

    ;; exif -c -o z_e26e7184.jpg --ifd=EXIF -t0x9286 --set-value=foo z_e26e7184.jpg
    (if (null sha-changed)
        (picp-bold "No files with changed sha1 checksum found.\n\n")
      (let ((n (length sha-changed)))
        (picp-db-update-command [?s]
          (lambda ()
            (picp-bold "The following %s file%s have changed %s sha1 checksum.\n"
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
            (picp-bold "The following %s redundant file name%s were not found on disk.\n"
                       n (picp-plural-s n))
            (insert "Their database entries contains at least one other file that do exist.\n"
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
            (picp-bold "The following %s unique file name%s were not found on disk.\n"
                       n (picp-plural-s n))
            (insert "Their database entries do not contain any existing files.\n"
                    "Type ")
            (picp-bold "u")
            (insert " to remove these entries from the picpocket database.\n"
                    "(Their database entries will be removed.)\n\n")
            (picp-insert-file-list unique-file-missing))
          (lambda ()
            (picp-remove-file-names-in-db unique-file-missing)
            (picp-bold "Removed %s missing unique file name%s and their entries from database."
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
            (picp-separate (list (car entry)
                                 (picp-format-tags (cadr entry))))
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
  (insert ";; If you plan to manually edit this file you should fist\n")
  (insert ";; kill the *picpocket* buffer in Emacs.  Otherwise your\n")
  (insert ";; edits may be overwritted.\n\n"))

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
  (format "should be %s" (mapconcat #'symbol-name picp-db-valid-formats " or ")))


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
(defvar picp-min-idle-secs 0.2)
(defvar picp-idle-timer-work-functions '((picp-maybe-save-journal 0)
                                         (picp-look-ahead-next 0)
                                         ;; (picp-look-ahead-more 2)
                                         (picp-compute-sha 4)
                                         (picp-save-journal 60)))
(defvar picp-inhibit-timers nil)
(defvar picp-idle-timer-deadline 0.1)


(defun picp-init-timers ()
  (if picp-inhibit-timers
      (picp-cancel-timers)
    (picp-cancel-timers)
    (add-hook 'kill-buffer-hook #'picp-cancel-timers nil t)
    (setq picp-timers (cl-loop for (f s) in picp-idle-timer-work-functions
                               collect (run-with-idle-timer (max picp-min-idle-secs
                                                                 s)
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
        (resume-timer (get f 'picp-resume-timer)))
    (when (timerp resume-timer)
      (cancel-timer resume-timer))
    (catch 'done
      (unless (get-buffer picp-buffer)
        (throw 'done (picp-cancel-timers)))
      (with-current-buffer picp-buffer
        (unless picp-list
          (throw 'done (picp-cancel-timers)))
        (unless (file-directory-p default-directory)
          (message "Closing picpocket buffer since %s do not exist any more."
                   default-directory)
          (kill-buffer picp-buffer)
          (throw 'done (picp-cancel-timers)))
        (condition-case err
            (funcall f (picp-make-resume-function f) state)
          (quit (message "picp-run-idle-timer %s interrupted by quit" f)
                (signal (car err) (cdr err))))))))

(defun picp-make-resume-function (f)
  (let ((start (current-time)))
    (lambda (state)
      (when (time-less-p (seconds-to-time picp-idle-timer-deadline)
                         (time-subtract (current-time) start))
        (put f
             'picp-resume-timer
             (run-with-idle-timer (time-add (or (current-idle-time) (seconds-to-time 0))
                                            (seconds-to-time picp-idle-timer-deadline))
                                  nil
                                  #'picp-run-idle-timer
                                  f
                                  state))))))

;; (defvar picp-t nil)
;; (cancel-timer picp-t)
;; (setq picp-inhibit-timers nil)
;; (with-current-buffer picp-buffer
;; (cl-loop for pic on picp-list
;; count 1
;; while (picp-sha pic)))
;;
;; (with-current-buffer picp-buffer (picp-sha (last picp-list)))

;; (setq picp-t (run-with-idle-timer 1 t #'picp-run-idle-timer #'picp-compute-sha))
;; (get 'picp-compute-sha 'picp-resume-timer)
;; (setq picp-idle-timer-deadline 0.1)
;; (picp-run-idle-timer #'picp-compute-sha)
;; (picp-compute-sha #'ignore)
;; (vectorp (get 'picp-compute-sha 'picp-resume-timer))

(defun picp-compute-sha (resume-function state)
  (with-current-buffer picp-buffer
    (let ((i 0))
      (cl-loop for pic on (or state picp-list)
               do (picp-safe-sha pic)
               do (cl-incf i)
               until (funcall resume-function pic)))))

(defun picp-maybe-save-journal (&rest ignored)
  (when (> (picp-db-journal-size) 100)
    (picp-db-save)))

(defun picp-save-journal (&rest ignored)
  (unless (zerop (picp-db-journal-size))
    (picp-db-save)))

(defun picp-look-ahead-next (&rest ignored)
  (when picp-current
    (let ((next (or (cdr picp-current)
                    (picp-prev))))
      (when next
        (let ((s (picp-time-string
                   (picp-look-ahead next))))
          (when (zerop picp-look-count)
            (setq picp-look-count 1
                  picp-header-text (format "look-%s-[%s] " picp-look-count s))))))))

(defun picp-look-ahead-more (resume-function ignored)
  (cl-destructuring-bind (n s) (picp-measure-time (picp-look-ahead-more2 resume-function))
    (when (eq 1 picp-look-count)
      (setq picp-header-text (format "more-%s-[%s] " picp-look-count s)
            picp-look-count n))))


(defun picp-look-ahead-more2 (resume-function)
  ;; PENDING - look ahead backward if last command was picp-prev?
  (cl-loop for pic on picp-current
           for count = 0 then (1+ count)
           until (funcall resume-function nil)
           finally return count
           repeat picp-look-ahead-max
           do (picp-look-ahead pic)))


;;; Buffer functions.

(defun picp-update-buffer ()
  (let ((s (picp-time-string (picp-do-update-buffer))))
    (when picp-debug
      (setq picp-header-text (format "%s-%s " this-command s)
            picp-look-count 0))))

(defun picp-do-update-buffer ()
  (unless (equal (buffer-name) picp-buffer)
    (error "This requires picpocket mode"))
  (if (null picp-current)
      (picp-no-images)
    (if (picp-ensure-matching-current-pic)
        (progn
          (picp-insert picp-current)
          (cd (picp-dir)))
      (picp-no-images)))
  (force-mode-line-update))

(defun picp-ensure-matching-current-pic ()
  (or (picp-filter-match-p picp-current)
      (let ((pic (or (picp-next-pic) (picp-previous-pic))))
        (when pic
          (let ((inhibit-quit t))
            (setq picp-current pic
                  picp-index (picp-calculate-index)))))))


(defun picp-calculate-index (&optional current)
  (cl-loop for pic on picp-list
           count 1
           until (eq pic (or current picp-current))))

(defun picp-no-images ()
  (if picp-dired-when-no-images-p
      (dired default-directory)
    (let (buffer-read-only)
      (erase-buffer)
      (insert (propertize (format "\n\nNo images in list%s.\n\n"
                                  (if picp-filter
                                      (format " matching filter %s" picp-filter)
                                    ""))
                                  'face 'bold)
              (format "Hit %s for dired in %s.\n"
                      (picp-where-is 'picp-dired)
                      (abbreviate-file-name default-directory))
              (format "Hit %s to rebuild list%s.\n"
                      (picp-where-is 'picp-revert)
                      (picp-file-count-estimate)))
      (when (eq picp-entry-function 'picpocket-dir)
        (insert (format "Hit %s to %s.\n"
                        (picp-where-is 'picp-toggle-recursive)
                        (if picp-recursive
                            "only look for images in current directory."
                          "recursively include images in subdirectories.")))))))

(defun picp-where-is (command)
  (let ((binding (where-is-internal command overriding-local-map t)))
    (propertize (if binding
                    (key-description binding)
                  (concat "M-x " (symbol-name command)))
                'face 'bold)))



(defun picp-create-buffer (files &optional selected-file dir)
  (unless (picp-imagemagick-p)
    (error "Picpocket requires Emacs compiled with imagemagick support"))
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
      (quit (picp-reset)
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
            (insert-image (picp-create-image pic (picp-window-size)))
        (insert "\n\nThis display does not support images.")))
    (goto-char (point-min))))

(defun picp-window-size ()
  ;; (if fullscreen
  ;; (setq picp-window-size (cons (display-pixel-width)
  ;; (display-pixel-height))
  (cl-destructuring-bind (x0 y0 x1 y1) (window-inside-pixel-edges)
    ;; Save the current window size for the benefit of timer functions
    ;; that do not necessarily run with the picpocket window selected.
    (setq picp-window-size (cons (- x1 x0) (- y1 y0)))))


(defun picp-look-ahead (pic)
  (image-size (picp-create-image pic picp-window-size)
              t
              (if (and picp-frame
                       (frame-live-p picp-frame))
                  picp-frame
                (selected-frame))))

(defun picp-size-param (pic window-size)
  (let* ((window-width (car window-size))
         (window-height (cdr window-size))
         (window-ratio (/ (float window-width)
                          window-height))
         (pic-size (picp-image-size pic))
         (pic-width (car pic-size))
         (pic-height (cdr pic-size))
         (pic-ratio (/ (float pic-width) pic-height)))
    (pcase picp-fit
      (:width-and-height (if (> window-ratio pic-ratio)
                             (cons :height (picp-scale window-height))
                           (cons :width (picp-scale window-width))))
      (:width  (cons :width (picp-scale window-width)))
      (:height (cons :height (picp-scale window-height)))
      (_       (cons :width (picp-scale pic-width))))))

(defun picp-scale (n)
  (/ (* picp-scale n) 100))

(defun picp-alter-scale (delta)
  (setq picp-scale
        (max 10 (+ picp-scale delta)))
  (message "Scaling factor is %s%%" picp-scale))

(defun picp-scale-info ()
  (unless (eq picp-scale 100)
    (format "%s%%%%" picp-scale)))

(defun picp-create-image (pic window-size)
  (let ((size-param (picp-size-param pic window-size)))
    (create-image (picp-path pic)
                  'imagemagick nil
                  :rotation (picp-rotation pic)
                  (car size-param) (cdr size-param))))
;; :foreground "white"
;; :background "black"



(defun picp-image-size (pic)
  (or (picp-size pic)
      (picp-set-size pic (image-size (create-image (picp-path pic)
                                                   'imagemagick)
                                     t))))

(defun picp-imagemagick-p ()
  (picp-image-regexp))

(defun picp-image-regexp ()
  (or picp-image-regexp
      (setq picp-image-regexp (car (rassq 'imagemagick
                                          image-type-file-name-regexps)))))


;;; File count estimate functions

(defun picp-file-count-estimate ()
  (cl-case picp-entry-function
    (picpocket-dir (picp-file-count-estimate-dir))
    (picpocket-files (picp-file-count-estimate-list))
    (t "")))

(defun picp-file-count-estimate-dir ()
  (let* ((dir (car picp-entry-args))
         (count (if picp-recursive
                    (picp-file-count-with-timeout dir 1)
                  (picp-nr-of-images-in-dir dir))))
    (if (and (listp count)
             (eq 'interrupted (car count)))
        (picp-format-estimate (cdr count) "before timeout")
      (picp-format-estimate count))))


(defun picp-file-count-with-timeout (dir timeout)
  (let* ((deadline (time-add (current-time) (seconds-to-time timeout)))
         (count (list 0)))
    (catch 'timeout
      (save-match-data
        (picp-file-count-with-deadline dir count deadline)
        (car count)))))

(defun picp-file-count-with-deadline (dir count deadline)
  (if (time-less-p deadline (current-time))
      (throw 'timeout (cons 'interrupted (car count)))
    (cl-incf (car count) (picp-nr-of-images-in-dir dir))
    (cl-loop for file in (directory-files dir t)
             do (when (and (file-directory-p file)
                           (not (string-match "/\\.?\\.$" file)))
                  (picp-file-count-with-deadline file count deadline)))))

(defun picp-nr-of-images-in-dir (dir)
  (cl-loop for file in (directory-files dir)
           when (and (not (file-directory-p file))
                     (string-match (picp-image-regexp) file))
           count 1))

(defun picp-file-count-estimate-list ()
  (picp-format-estimate
   (cl-loop for file in (car picp-entry-args)
            when (and (string-match (picp-image-regexp) file)
                      (file-exists-p file))
            count 1)))

(defun picp-format-estimate (count &optional suffix)
  (format " (%s image file%s found%s)"
          count
          (picp-plural-s count)
          (or suffix "")))


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
  (let* ((prompt (format "Type a keystroke to select tag to %s %s(type ? for help): "
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
                                   (key-description (vconcat prefix (vector key)))
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
                      ((memq action '(move copy hard-link))
                       (picp-other-command action arg))
                      ((symbolp action)
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
                    (picp-action 'tag ,tag)))
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
  ;; (let ((picpocket (picp-visible-window picp-buffer)))
  ;; (if picpocket
  ;; (select-window picpocket)
  ;; (switch-to-buffer picp-buffer))))

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
                      (picp-describe-binding binding)
                      (when (eq binding 'picp-repeat)
                        (picp-repeat-help))
                      (princ "\n")))))))

(defun picp-repeat-help ()
  (princ (format " (%s %s%s)"
                 picp-last-action
                 picp-last-arg
                 (if picp-last-arg-is-dir "/" ""))))

(defun picp-describe-binding (binding)
  ;; (insert (symbol-name binding)))
  ;; insert-text-button is actually only needed for picp-repeat.
  (insert-text-button (symbol-name binding)
                      'type 'help-function
                      'help-args (list binding)))

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
             (user-error "Attempt to %s file to itself" (picp-action-string action)))
            ((picp-files-identical-p old-path new-path)
             (setq ok-if-already-exists
                   (y-or-n-p (format "Identical file already exists in %s.  Overwrite? "
                                     new-dir))
                   identical t))
            ((file-directory-p new-path)
             (setq new-file (read-string
                             (format "Directory %s already exists.  Rename this to: "
                                     new-path)
                             old-file)
                   new-path (concat new-dir new-file)))
            (t
             (picp-compare pic new-path)
             (setq new-file (read-string
                             (format "File already exists (size %s).  Rename this (size %s) to: "
                                     (picp-file-kb new-path)
                                     (picp-file-kb old-path))
                             old-file)
                   new-path (concat new-dir new-file)))))
    (cl-case action
      (move
       (picp--move old-path new-path ok-if-already-exists pic))
      ((copy hard-link)
       (picp--duplicate action old-path new-path ok-if-already-exists))
      (t (error "Invalid picpocket action %s" action)))))

(defun picp-action-string (action)
  (cl-case action
    (copy "copy")
    (move "rename")
    (hard-link "hard link")))

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
  (cl-destructuring-bind (window-width . window-height) (picp-window-size)
    (let* ((line-height (+ (frame-char-height)
                           (or line-spacing
                               (frame-parameter nil 'line-spacing)
                               0)))
           (pic-height (/ (- window-height (* 2 line-height)) 2))
           (picp-fit 'width-and-height)
           (picp-scale 100)
           buffer-read-only)
      (erase-buffer)
      (insert (format "About to overwrite this picture (%s):\n" (picp-file-kb new)))
      (insert-image (picp-create-image (picp-make-pic new)
                                       (cons window-width pic-height)))
      (insert (format "\nWith this picture (%s):\n" (picp-pic-kb pic)))
      (insert-image (picp-create-image pic (cons window-width pic-height)))
      (goto-char (point-min)))))


;;; Header line functions

(defun picp-header-line ()
  (when picp-current
    (picp-separate
     (list (concat (format "%s/%s " picp-index picp-length)
                   (picp-header-dir)
                   "/"
                   (propertize (picp-file) 'face 'highlight))
           (when picp-debug
             picp-header-text)
           (picp-pic-kb picp-current)
           (picp-scale-info)
           (picp-format-tags (picp-tags picp-current))
           (when picp-filter
             (format "filter: %s" picp-filter))))))

(defun picp-separate (list)
  (mapconcat 'identity
             (delete nil (delete "" list))
             " "))

(defun picp-header-dir ()
  (if picp-header-full-path
      ;; abbreviate-file-name substitutes the users home
      ;; directory with "~".  This do not work if the home
      ;; directory is a symbolic link.  The below addition to
      ;; directory-abbrev-alist makes it work also for that
      ;; case.
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

(defun picp-maybe-space (string)
  (if (and string
           (not (string-equal string "")))
      (concat " " string)
    ""))

(defun picp-format-tags (tags)
  (when tags
    (cl-case picp-tags-style
      (org (format ":%s:" (mapconcat #'symbol-name tags ":")))
      (t (format "(%s)" (mapconcat #'symbol-name tags " "))))))


;;; Misc help functions

(defun picp-file-list (dir recursive)
  (save-match-data
    (cl-loop for file in (directory-files (file-truename dir) t)
             append (if (file-directory-p file)
                        (when recursive
                          (unless (string-match "/\\.?\\.$" file)
                            (picp-file-list file t)))
                      (when (string-match (picp-image-regexp) file)
                        (list file))))))


(defun picp-y-or-n-p (format &rest objects)
  (let* ((prompt (apply #'format format objects))
         (header-line-format (concat prompt " (y or n)")))
    (y-or-n-p prompt)))


(defun picp-window-size-change-function (frame)
  (dolist (window (window-list frame 'no-minibuffer))
    (when (eq (get-buffer picp-buffer) (window-buffer window))
      (with-selected-window window
        (with-current-buffer picp-buffer
          (picp-update-buffer))))))


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
  ;; PENDING - should compare 512 bytes or so at a time.
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

(provide 'picpocket)

;;; picpocket.el ends here
