;;; picpocket-test.el -*- lexical-binding: t; coding: utf-8-unix -*-

;; Copyright (C) 2013 Johan Claesson
;; Author: Johan Claesson <johanclaesson@bredband.net>
;; Created:    <2013-03-03>
;; Time-stamp: <2017-02-06 11:02:52 jcl>
;; Version: 27

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

;;; Code:



(require 'ert)
(require 'cl-lib)
(require 'time-date)
(require 'subr-x)

(cl-eval-when (compile)
  (require 'picpocket (concat default-directory "picpocket.el")))

(cl-eval-when (eval load)
  (require 'picpocket (concat (file-name-directory (or load-file-name
                                                       (buffer-file-name)))
                              "picpocket.el")))



(defconst picp-test-files (list "blue.svg" "green.svg" "red.svg"))
(defconst picp-test-tree-files (list "green.svg"
                                     "cold/blue.svg"
                                     "warm/red.svg"))
(defvar picp-test-dir nil)
(defvar picp-delete-dir-after-test t)
(put 'picp-test-dir 'risky-local-variable t)

(defconst picp-expected-files
  (append (list "picpocket-db-journal.el"
                "picpocket-db-old.el"
                "picpocket-db.el"
                "picpocket-db-tmp.el")
          picp-test-files))

(defconst picp-expected-dirs
  (append (cl-loop for file in picp-test-tree-files
                   for dir = (file-name-directory file)
                   when dir
                   collect dir)
          (list "manga-dir/")))

(defvar picp-clock-file "~/share/elisp/picpocket-clock.txt")

(defun picp-add-tag-manga ())
(defun picp-add-tag-horror ())
(defvar picp-test-keystroke-alist '((?M add-tag "manga")
                                    (?H add-tag "horror")))


;;; Macros


(defmacro picp-with-test-dir (&rest body)
  "Run test in directory with blue.svg, green.svg and red.svg."
  (declare (debug (body))
           (indent defun))
  `(let* ((picp-test-dir (or picp-test-dir
                             (file-name-as-directory
                              (make-temp-file "picpocket-test-" t))))
          (picp-db-dir picp-test-dir)
          (picp-db nil)
          (picp-db-journal-size 0)
          (picp-inhibit-timers t)
          (picp-demote-warnings t)
          (picp-destination-relative-current t)
          (picp-recursive t)
          (picp-keystroke-alist 'picp-test-keystroke-alist)
          (picp-undo-ring nil)
          (picp-trashcan nil)
          (default-directory picp-test-dir))
     (get-buffer-create picp-undo-buffer)
     (make-directory picp-test-dir t)
     (unwind-protect
         (prog2
             (picp-ensure-only-test-files)
             (progn ,@body)
           (picp-check-only-expected-files))
       (kill-buffer picp-undo-buffer)
       (when picp-trashcan
         (delete-directory picp-trashcan t))
       (when picp-delete-dir-after-test
         (delete-directory picp-test-dir t)))))

(defmacro picp-with-test-buffer (&rest body)
  "Run picpocket test starting at blue.svg in directory with
blue.svg, green.svg and red.svg."
  (declare (debug (body))
           (indent defun))
  `(picp-with-test-dir
     (with-current-buffer (picpocket-dir default-directory "blue.svg")
       (prog1
           (progn ,@body)
         (when (buffer-live-p (get-buffer picp-buffer))
           (kill-buffer picp-buffer))))))


(defmacro picp-with-test-dir-tree (&rest body)
  "Run test in this tree:

green.svg
cold/blue.svg
warm/red.svg"
  (declare (debug (body))
           (indent defun))
  `(picp-with-test-dir
     (make-directory "cold")
     (rename-file "blue.svg" "cold")
     (make-directory "warm")
     (rename-file "red.svg" "warm")
     ,@body))

(defmacro picp-with-test-buffer-tree (&rest body)
  "Run picpocket test starting at green.svg in this tree:

green.svg
cold/blue.svg
warm/red.svg"
  (declare (debug (body))
           (indent defun))
  `(picp-with-test-dir-tree
     (with-current-buffer (picpocket-dir default-directory "green.svg")
       (prog1
           (progn ,@body)
         (kill-buffer picp-buffer)))))



(defmacro picp-report-time (&rest body)
  (declare (debug (body))
           (indent defun))
  `(cl-destructuring-bind (rc time)
       (picp-time ,@body)
     (message "Something in testcase %s took %s"
              (if ert--running-tests
                  (ert-test-name (car ert--running-tests))
                "none")
              (picp-sec-string time))
     rc))


;;; Benchmark

(defun picp-release-bench ()
  (let ((picp-clock-file "~/share/elisp/picpocket-clock-release.txt"))
    (picp-bench)))

;; PENDING - picp-file-list bench
;; PENDING - Make it possible to compare older versions
;; Probably should put git tags on each version.
(defun picp-bench ()
  (let ((imagemagick-render-type 1))
    (picp-with-clock "Default benchmark"
      (picp-clock (picpocket-dir "~/bilder/japan/dvd/"))
      (with-current-buffer picp-buffer
        (dotimes (ignored 30)
          (picp-clock (picp-look-ahead-next))
          (picp-clock (redisplay))
          (picp-clock (picp-next)))))))

(defun picp-clock-report (title)
  (cl-loop for (thing . s) in picp-clock-alist
           do (message "%s %s" thing (picp-sec-string s)))
  (when picp-clock-file
    (with-current-buffer (let ((enable-local-variables :safe))
                           (find-file-noselect picp-clock-file))
      (goto-char (point-max))
      (insert "\n")
      (pp (append (picp-clock-info title)
                  picp-clock-alist)
          (current-buffer))
      (save-buffer))))

(defun picp-clock-info (title)
  (list (cons 'title title)
        (cons 'time (current-time))
        (cons 'cpu (picp-cpu))
        (cons 'linux (string-trim (shell-command-to-string
                                   "lsb_release -ds")))
        (cons 'emacs-version (cons emacs-version
                                   (emacs-repository-get-version)))
        (cons 'imagemagick-version (picp-imagemagick-version))
        (cons 'byte-compile (not (not (member ".elc" load-suffixes))))
        (cons 'render-type imagemagick-render-type)
        (cons 'picp-version picp-version)))

(defun picp-imagemagick-version ()
  (string-trim (cadr (split-string (shell-command-to-string
                                    "dpkg -s imagemagick | grep Version")
                                   " "))))

(defun picp-cpu ()
  (cadr (split-string (shell-command-to-string "lscpu  | grep 'Model name'")
                      ":" t "[\n\t ]+")))


(ert-deftest picp-test-picp-clock-thing ()
  (let (picp-clock-alist)
    (cl-flet ((picp-time (&rest _forms) (list 1 (seconds-to-time 1))))
      (picp-clock-thing :plus (+ 1 1))
      (picp-clock-thing :minus (- 1 1))
      (picp-clock-thing :plus (+ 1 1))
      (should (equal  (list (cons :minus (seconds-to-time 1))
                           (cons :plus (seconds-to-time 2)))
                     picp-clock-alist)))))

(ert-deftest picp-test-picp-clock ()
  (let (picp-clock-alist)
    (cl-flet ((picp-time (&rest _forms) (list 1 (seconds-to-time 1))))
      (picp-clock (+ 1 1))
      (picp-clock (- 1 1))
      (picp-clock (+ 1 1))
      (should (equal (list (cons '- (seconds-to-time 1))
                           (cons '+ (seconds-to-time 2)))
                     picp-clock-alist)))))

;; PENDING
;; Ignore the byte-compile field.  Maybe delete those old entries.
;; Field is-current-emacs.
;; Normally only is-current-emacs=t entries are shown.
;; Prefix arg shows all.

(defun picp-list-bench (keys byte-compiled)
  (interactive
   (list (mapcar #'intern (split-string (picp-bench-read-keys) nil t))
         (y-or-n-p "Byte-compiled? ")))
  (save-excursion
    (let (alists
          pruned-alists
          old-alist)
      (goto-char (point-min))
      (while (progn
               (skip-chars-forward " \t\n")
               (not (eobp)))
        (let ((alist (read (current-buffer))))
          (when (eq byte-compiled (cdr (assq 'byte-compile alist)))
            (push alist alists))))
      (dolist (alist (nreverse alists))
        (push (cl-loop for entry in alist
                       unless (equal entry (assq (car entry) old-alist))
                       collect entry)
              pruned-alists)
        (setq old-alist alist))
      (display-buffer
       (with-current-buffer (get-buffer-create "*Picp-bench*")
         (erase-buffer)
         (dolist (x (nreverse pruned-alists))
           (insert "\n")
           (picp-pp (picp-filter-alist x keys)))
         (current-buffer))))))

(defun picp-pp (alist)
  (cl-loop for (key . value) in alist
           do (picp-pp2 key value)))

(defun picp-pp2 (key value)
  (insert (format "%-20s %s\n"
                  key
                  (cond ((and (listp value)
                              (eq 4 (safe-length value)))
                         (if (zerop (car value))
                             (format "%.2fs" (time-to-seconds value))
                           (current-time-string value)))
                        (t value)))))

(defun picp-filter-alist (alist keys)
  (if keys
      (cl-loop for entry in alist
               when (memq (car entry) keys)
               collect entry)
    alist))

(defun picp-bench-read-keys ()
  (picp-read-with-completion "Show these fields (RET for all): "
                             (picp-key-collection)))

(defun picp-read-with-completion (prompt
                                  collection
                                  &optional initial-contents)
   (minibuffer-with-setup-hook
       (lambda ()
         (setq-local completion-at-point-functions
                     (list (lambda ()
                             (list (save-excursion
                                     (skip-chars-backward "^ ")
                                     (point))
                                   (point)
                                   collection)))))
     (read-from-minibuffer prompt
                           initial-contents
                           picp-minibuffer-map)))

(defun picp-key-collection ()
  (save-excursion
    (goto-char (point-min))
    (read (current-buffer))))



;;; Help functions

(defun picp-list-should-be-reset ()
  (should (eq picp-list nil))
  (should (eq picp-current nil))
  (should (eq picp-index 0))
  (should (eq picp-list-length 0)))


(defun picp-files ()
  (cl-loop for pic on picp-list
           collect (picp-relative-dir (concat (picp-dir pic)
                                              (picp-file pic)))))

(defun picp-relative-dir (dir)
  (substring dir (length (file-truename default-directory))))

(defun picp-ensure-only-test-files ()
  (when (file-exists-p picp-test-dir)
    (delete-directory picp-test-dir t))
  (make-directory picp-test-dir t)
  (let ((default-directory picp-test-dir))
    (dolist (file picp-test-files)
      (unless (file-exists-p file)
        (with-temp-file file
          (picp-insert-svg file))))))

(defun picp-insert-svg (file)
  (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n"
          "  <rect width=\"150\" height=\"150\" fill=\"rgb("
          (picp-svg-color file) ")\"\n"
          "   stroke-width=\"1\" stroke=\"rgb(0, 0, 0)\"/>\n"
          "</svg>\n"))

(defun picp-svg-color (file)
  (pcase (file-name-sans-extension file)
    ("red"   "255, 0, 0")
    ("green" "0, 255, 0")
    ("blue"  "0, 0, 255")))


(defun picp-check-only-expected-files ()
  (let ((default-directory picp-test-dir))
    (dolist (file (directory-files default-directory nil nil t))
      (unless (or (equal file ".")
                  (equal file ".."))
        (if (file-directory-p file)
            (unless (member (file-name-as-directory file) picp-expected-dirs)
              (error "Unexpected directory %s in %s"
                     file default-directory))
          (unless (member file picp-expected-files)
            (error "Unexpected file %s in %s"
                   file default-directory)))))))







;;; Test cases.

(ert-deftest picp-tree-add-tag-to-all ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (picp-report-time (picp-edit-tags 'all "manga"))
    (should (equal (picp-tags) '(manga)))
    (should (equal (picp-tags (cdr picp-current)) '(manga)))
    (should (equal (picp-tags (cddr picp-current)) '(manga)))))

(ert-deftest picp-tag-to-all ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-report-time (picp-edit-tags 'all "manga"))
    (should (equal (picp-tags) '(manga)))
    (should (equal (picp-tags (cdr picp-current)) '(manga)))
    (should (equal (picp-tags (cddr picp-current)) '(manga)))))

(ert-deftest picp-tree-move-within-tree ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (picp-report-time
      (picp-next)
      (picp-edit-tags nil "manga")
      (picp-rename "../warm"))
    (should (equal (picp-dir) (file-truename default-directory)))
    (should (equal (picp-file) "red.svg"))
    (should (file-exists-p "../warm/blue.svg"))))


(ert-deftest picp-tree-copy-within-tree ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (picp-report-time
      (picp-next)
      (picp-add-tag-manga)
      (picp-copy nil "../warm"))
    (should (equal 3 picp-list-length))
    (should (equal 3 (length picp-list)))
    (should (equal (picp-dir) (file-truename default-directory)))
    (should (equal (picp-file) "blue.svg"))
    (should (equal (picp-tags) '(manga)))
    (should (file-exists-p "blue.svg"))
    (picp-revert)
    (with-current-buffer picp-buffer
      (should (equal 4 picp-list-length)))))

(ert-deftest picp-tree-move-outside-tree ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (picp-report-time
      (picp-next)
      (picp-add-tag-manga)
      (let* ((outside (file-name-as-directory
                       (make-temp-file "picp-test-" t)))
             (outside-blue (concat outside "blue.svg")))
        (unwind-protect
            (progn
              (picp-move nil outside)
              (should (file-exists-p outside-blue)))
          (delete-file outside-blue)
          (delete-directory outside))))))

(ert-deftest picp-tree-add-and-clear-tag ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (picp-report-time
      (picp-next)
      (picp-add-tag-manga))
    (should (equal (picp-tags) '(manga)))
    (picp-report-time
      (picp-tags-delete-file picp-current (picp-absfile)))
    (should-not (picp-tags))))

(ert-deftest picp-tree-make-list-with-tags ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (picp-add-tag-manga)
    (picp-create-picp-list (picp-file-list ".")
                           (file-truename "green.svg"))
    (should (equal (picp-tags)
                   '(manga)))))


(ert-deftest picp-tree-make-list-with-tags2 ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (picp-add-tag-manga)
    (picp-next)
    (picp-add-tag-horror)
    (picp-report-time
      (picp-create-picp-list (picp-file-list "..")
                             (file-truename "../green.svg")))
    (should (equal (picp-tags)
                   '(manga)))
    (should (equal (picp-tags (cdr picp-current))
                   '(horror)))))

(ert-deftest picp-tree-file-list ()
  :tags '(:picpocket)
  (picp-with-test-dir-tree
    (should (equal (picp-report-time (picp-file-list default-directory))
                   (mapcar #'file-truename picp-test-tree-files)))))

(ert-deftest picp-file-list-symlink-loop ()
  :tags '(:picpocket)
  (picp-with-test-dir-tree
    (unwind-protect (progn
                      (make-symbolic-link "." "self")
                      (make-symbolic-link "../warm" "cold/w")
                      (make-symbolic-link "../cold" "warm/c")
                      (should (eq 3 (length (picp-file-list picp-test-dir)))))
      (delete-directory picp-test-dir t)
      (make-directory picp-test-dir))))

(ert-deftest picp-file-list-dot-file ()
  :tags '(:picpocket)
  (picp-with-test-dir-tree
    (unwind-protect (progn
                      (make-directory ".dot")
                      (copy-file "green.svg" ".dot")
                      (should (eq 3 (length (picp-file-list
                                             picp-test-dir)))))
      (delete-directory (concat picp-test-dir ".dot") t))))


(ert-deftest picp-files-test ()
  :tags '(:picpocket)
  (picp-report-time
    (picp-with-test-buffer
      (should (equal picp-test-files
                     (picp-files)))
      (should (equal picp-list picp-current))
      (should (equal 3 picp-list-length))
      (should (equal 1 picp-index)))))

(ert-deftest picp-tree-files ()
  :tags '(:picpocket)
  (picp-report-time
    (picp-with-test-buffer-tree
      (should (equal picp-test-tree-files
                     (picp-files)))
      (should (eq picp-list picp-current))
      (should (equal 3 picp-list-length))
      (should (equal 1 picp-index)))))


(ert-deftest picp-add-tag-delete-file ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-report-time
      (picp-add-tag-manga)
      (picp-delete-file))
    (should-not (file-exists-p "blue.svg"))
    (should-not (picp-tags))))


(ert-deftest picp-delete-from-beginning ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-dump)
    (picp-delete-file)
    (picp-dump)
    (should (eq 2 (length picp-list)))
    (picp-delete-file)
    (picp-dump)
    (should (eq 1 (length picp-list)))
    (picp-delete-file)
    (picp-dump)
    (picp-list-should-be-reset)))

(ert-deftest picp-delete-from-the-middle-and-end ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-next)
    (picp-delete-file)
    (should (eq 2 (length picp-list)))
    (picp-delete-file)
    (should (eq 1 (length picp-list)))
    (picp-delete-file)
    (picp-dump)
    (picp-list-should-be-reset)))

(ert-deftest picp-move-all ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-move 'all "warm")
    (should (not picp-current))
    (should (eq (+ 2 3) (length (directory-files "warm"))))))

(ert-deftest picp-copy-all ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-next)
    (picp-copy 'all "warm")
    (should (eq 3 (length picp-list)))
    (should (eq (+ 2 3) (length (directory-files "warm"))))))

(ert-deftest picp-hardlink-all ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-end)
    (picp-hardlink 'all "warm")
    (should (eq 3 (length picp-list)))
    (should (eq (+ 2 3) (length (directory-files "warm"))))))


(defun picp-db-put-test-data ()
  (picp-db-init)
  (picp-db-put "1" "Some data")
  (picp-db-put "2" "Some more data")
  (picp-db-put "1" nil))

(defun picp-db-verify-test-data ()
  (should (equal "Some more data" (picp-db-get "2"))))




(defmacro picp-with-test-db (&rest body)
  (declare (debug (body))
           (indent defun))
  `(cl-loop for picp-db-format in picp-db-valid-formats
            collect (picp-with-test-dir
                      (picp-db-put-test-data)
                      ,@body)))

(ert-deftest picp-db-put-get-test ()
  (picp-with-test-db
    (picp-db-verify-test-data)))

(ert-deftest picp-db-read-database-file-test ()
  (picp-with-test-db
    (picp-db-save)
    (setq picp-db nil)
    (picp-db-init)
    (picp-db-verify-test-data)))

(ert-deftest picp-db-read-database-file-with-corrupt-fallback-test ()
  (picp-with-test-db
    (picp-db-save)
    (setq picp-db nil)
    (with-temp-file (picp-db-file :old)
      (insert "Typical corrupt data...\n"))
    (picp-db-init)
    (picp-db-verify-test-data)))

(ert-deftest picp-db-read-fallback-file-test ()
  (picp-with-test-db
    (picp-db-save)
    (setq picp-db nil)
    (rename-file (picp-db-file)
                 (picp-db-file :old))
    (picp-db-init)
    (picp-db-verify-test-data)))

(ert-deftest picp-db-read-fallback-instead-of-corrupt-database-file-test ()
  (picp-with-test-db
    (picp-db-save)
    (setq picp-db nil)
    (rename-file (picp-db-file)
                 (picp-db-file :old))
    (with-temp-file (picp-db-file)
      (insert "Typical corrupt data...\n"))
    (picp-db-init)
    (picp-db-verify-test-data)))

(ert-deftest picp-db-read-journal-test ()
  (picp-with-test-db
    (setq picp-db nil)
    (picp-db-init)
    (picp-db-verify-test-data)))


(ert-deftest picp-db-tags-test ()
  (cl-labels ((expect-files (&rest files)
                            (should (picp-lists-equal (picp-db-files "sha1")
                                                      files)))
              (expect-tags (tags)
                           (should (picp-lists-equal (picp-db-tags "sha1")
                                                     tags))))
    (picp-with-test-dir
      (picp-db-clear)
      (picp-db-tags-set "sha1" "a" '(cool fonzy))
      (picp-db-tags-add-file "sha1" "b")
      (expect-files "a" "b")

      (picp-db-tags-set "sha1" "c" '(something else))
      (expect-files "a" "b" "c")
      (expect-tags '(something else))

      (picp-db-tags-set "sha1" "a" nil)
      (expect-tags nil)
      (should (zerop (picp-db-count)))

      (picp-db-tags-set "sha1" "a" '(other stuff))
      (picp-db-tags-move-file "sha1" "a" "b")
      (expect-files "b")

      (picp-db-tags-add-file "sha1" "c")
      (picp-db-tags-move-file "sha1" "b" "c")
      (expect-files "c")

      (picp-db-tags-copy-file "sha1" "d")
      (expect-files "c" "d")

      (picp-db-tags-add-file "sha1" "e")
      (picp-db-tags-copy-file "sha1" "c")
      (expect-files "c" "d" "e")

      (picp-db-tags-delete-file "sha1" "c")
      (expect-files "d" "e")
      (picp-db-tags-delete-file "sha1" "d")
      (expect-files "e")
      (picp-db-tags-delete-file "sha1" "e")
      (expect-files)
      (expect-tags nil)
      (should (zerop (picp-db-count))))))

(defun picp-lists-equal (a b)
  (equal (sort (cl-copy-list a) 'picp-lessp)
         (sort (cl-copy-list b) 'picp-lessp)))

(defun picp-lessp (a b)
  (string-lessp (prin1-to-string a) (prin1-to-string b)))



(ert-deftest picp-external-file-sha-change ()
  (picp-with-test-buffer
    (picp-add-tag "dragon")
    (append-to-file " " nil "blue.svg")
    (let ((sha-changed (cdr (assq :sha-changed (picp-db-traverse))))
          (sha (picp-sha1sum "blue.svg")))
      (should (eq 1 (length sha-changed)))
      (picp-update-sha sha-changed)
      (should (picp-db-get sha))
      (should (equal (plist-get (picp-db-get sha) :tags)
                     '(dragon))))))

(ert-deftest picp-delete-unique-tagged-file ()
  (picp-with-test-buffer
    (picp-add-tag "troll")
    (delete-file "blue.svg")
    (let ((unique-file-missing (cdr (assq :unique-file-missing
                                          (picp-db-traverse)))))
      (should (eq 1 (length unique-file-missing)))
      (picp-remove-file-names-in-db unique-file-missing)
      (should (zerop (picp-db-count))))))

(defun picp-add-tag (tag-string)
  (picp-command
    (picp-action 'add-tag tag-string)))

(ert-deftest picp-delete-redundant-tagged-file ()
  (picp-with-test-buffer
    (copy-file "blue.svg" "green.svg" t)
    (picp-add-tag "troll")
    (cl-loop for pic on picp-list
             do (picp-sha-force pic))
    (picp-db-dump)
    (should (eq 1 (picp-db-count)))
    (delete-file "blue.svg")
    (let ((redundant-file-missing (cdr (assq :redundant-file-missing
                                             (picp-db-traverse)))))
      (should (eq 1 (length redundant-file-missing)))
      (picp-remove-file-names-in-db redundant-file-missing))
    (should (eq 1 (picp-db-count)))))


(ert-deftest picp-test-pic-by-index ()
  (picp-with-test-buffer
    (should-not (picp-pic-by-index 0))
    (should (equal "blue.svg" (picp-file (picp-pic-by-index 1))))
    (should (equal "red.svg" (picp-file (picp-pic-by-index 3))))
    (should-not (picp-pic-by-index 4))))

(ert-deftest picp-test-jump ()
  (picp-with-test-buffer-tree
    (picp-jump-to-file "green.svg")
    (should (equal 1 picp-index))
    (picp-rename "red.svg")
    (let ((reds (picp-pos-list-by-file "red.svg")))
      (should (eq 2 (length reds)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest ignored) (cadr coll))))
        (let ((pic (picp-select-pos-by-dir reds "Choose wisely: ")))
          (should (eq 3 (picp-calculate-index pic))))))))


(ert-deftest picp-test-compute-filter-match-count ()
  (picp-with-test-buffer
    (should-not picp-filter-match-count-done)
    (should-not picp-filter-match-count)
    ;; No filter.
    (picp-compute-filter-match-count #'ignore nil)
    (should-not picp-filter-match-count-done)
    (should-not picp-filter-match-count)
    ;; No matches.
    (picp-set-filter "orc")
    (picp-compute-filter-match-count #'ignore nil)
    (should picp-filter-match-count-done)
    (should (zerop picp-filter-match-count))
    ;; Two matches.
    (picp-set-filter "")
    (picp-edit-tags nil "green orc maniac")
    (picp-next)
    (picp-edit-tags nil "orc")
    (picp-previous)
    (picp-set-filter "orc")
    (picp-compute-filter-match-count #'ignore nil)
    (should picp-filter-match-count-done)
    (should (eq 2 picp-filter-match-count))
    ;; Force rescan.
    (picp-set-filter "orc")
    (picp-compute-filter-match-count #'ignore nil)
    (should picp-filter-match-count-done)
    (should (eq 2 picp-filter-match-count))
    ;; Alter tags.
    (picp-edit-tags nil "troll berserk")
    (should (eq 1 picp-filter-match-count))
    (picp-edit-tags nil "vampire duke")
    (should (zerop picp-filter-match-count))
    ;; Delete file.
    (picp-set-filter "")
    (picp-home)
    (picp-edit-tags nil "orc horde")
    (picp-next)
    (picp-edit-tags nil "orc minion")
    (picp-set-filter "orc")
    (picp-compute-filter-match-count #'ignore nil)
    (should (eq 2 picp-filter-match-count))
    (picp-delete-file)
    (should picp-filter-match-count-done)
    (should (eq 1 picp-filter-match-count))
    (picp-delete-file)
    (should picp-filter-match-count-done)
    (should (zerop picp-filter-match-count))))

(ert-deftest picp-test-compute-filter-index ()
  (picp-with-test-buffer
    (should-not picp-filter-index)
    (picp-set-filter "orc")
    ;; No matches.
    (picp-compute-filter-index #'ignore nil)
    (should (zerop picp-filter-index))
    ;; Two matches.
    (picp-set-filter "")
    (picp-edit-tags nil "green orc maniac")
    (picp-next)
    (picp-edit-tags nil "orc")
    (picp-home)
    (picp-set-filter "orc")
    (picp-compute-filter-index #'ignore nil)
    (should (eq 1 picp-filter-index))
    (picp-next)
    (should (eq 2 picp-filter-index))
    ;; Force rescan.
    (picp-set-filter "orc")
    (picp-compute-filter-index #'ignore nil)
    (should (eq 2 picp-filter-index))
    ;; Alter tags.
    (picp-edit-tags nil "troll berserk")
    (should-not picp-filter-index)
    (picp-compute-filter-index #'ignore nil)
    (should (eq 1 picp-filter-index))
    ;; Delete.
    (picp-set-filter "")
    (picp-home)
    (picp-edit-tags nil "orc horde")
    (picp-next)
    (picp-edit-tags nil "orc minion")
    (picp-set-filter "orc")
    (picp-compute-filter-index #'ignore nil)
    (should (eq 2 picp-filter-index))
    (picp-delete-file)
    (should-not picp-filter-index)
    (picp-compute-filter-index #'ignore nil)
    (should (eq 1 picp-filter-index))
    (picp-delete-file)
    (should-not picp-filter-index)
    (picp-compute-filter-index #'ignore nil)
    (should (zerop picp-filter-index))))

(ert-deftest picp-empty-undo-buffer-test ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-visit-undo-list))
  (picp-with-test-buffer
    (let ((picp-undo-ring (make-ring picp-undo-list-size)))
      (picp-visit-undo-list))))

(ert-deftest picp-undo-buffer-test ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-delete-file)
    (picp-delete-file)
    (picp-delete-file)
    (picp-visit-undo-list)))

(ert-deftest picp-file-name-lessp-test ()
  (cl-loop for (a b) in '(("" "")
                          ("" "x")
                          ("x" "")
                          ("x" "x")
                          ("xxx" "yyy"))
           do (should (eq (string-lessp a b)
                          (picp-file-name-lessp a b))))
  (cl-loop for (a b) in '(("2" "10")
                          ("1.2" "1.10")
                          ("x2" "x10"))
           do (should (picp-file-name-lessp a b))))

(ert-deftest picp-trash-file-test ()
  (let ((picp-trashcan (file-name-as-directory
                        (make-temp-file "/tmp/picp-trash-file-test" t))))
    (dotimes (_ 4)
      (with-temp-file (picp-trash-file "foo.png")))
    (dolist (f '("foo.png" "foo_2.png" "foo_3.png" "foo_4.png"))
      (delete-file (expand-file-name f picp-trashcan)))
    (delete-directory picp-trashcan)))


(ert-deftest picp-single-undelete-test ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-add-tag-manga)
    (picp-delete-file)
    (picp-undo)
    (should (eq 3 (length picp-list)))
    (should (eq 3 picp-list-length))
    (should (file-exists-p "blue.svg"))
    (should (equal (picp-tags) '(manga)))))


(ert-deftest picp-undelete-all-test ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-delete-file)
    (picp-delete-file)
    (picp-delete-file)
    (picp-undo)
    (picp-undo)
    (picp-undo)
    (should (eq 3 (length picp-list)))
    (should (eq 3 picp-list-length))
    (should (file-exists-p "blue.svg"))
    (should (file-exists-p "green.svg"))
    (should (file-exists-p "red.svg"))))

(ert-deftest picp-undo-add-tag-test ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-add-tag-manga)
    (picp-undo)
    (should-not (picp-tags))))

(ert-deftest picp-undo-remove-tag-test ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-add-tag-manga)
    (picp-edit-tags nil "")
    (picp-undo)
    (should (equal (picp-tags) '(manga)))))

(ert-deftest picp-undo-rename-test ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-rename "bluish.svg")
    (should (equal (picp-file) "bluish.svg"))
    (picp-undo)
    (should (equal (picp-file) "blue.svg"))))

(ert-deftest picp-undo-move-test ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-add-tag-manga)
    (picp-move nil "subdir")
    (should (file-exists-p "subdir/blue.svg"))
    (should (eq 2 (length picp-list)))
    (picp-undo)
    (should (equal (picp-file) "blue.svg"))
    (should (file-exists-p "blue.svg"))
    (should (eq 3 (length picp-list)))
    (should (equal (picp-tags) '(manga)))
    (delete-directory "subdir")))

(ert-deftest picp-undo-copy-test ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (picp-add-tag-manga)
    (make-directory "subdir")
    (with-temp-file "subdir/blue.svg"
      (insert "some important document"))
    (let ((noninteractive t))
      (picp-copy nil "subdir"))
    (should (picp-files-identical-p "blue.svg" "subdir/blue.svg"))
    (picp-undo)
    (should (equal (picp-file-content "subdir/blue.svg")
                   "some important document"))
    (should (equal (picp-tags) '(manga)))
    (delete-directory "subdir" t)))



(provide 'picpocket-test)

;;; picpocket-test.el ends here
