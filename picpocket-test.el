;;; picpocket-test.el --- Tests -*- lexical-binding: t; coding: utf-8-unix -*-

;; Copyright (C) 2017 Johan Claesson
;; Author: Johan Claesson <johanclaesson@bredband.net>
;; URL: https://github.com/johanclaesson/picpocket
;; Version: 38
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

;; ERT tests for picpocket.el.

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



(defconst picpocket-test-files (list "blue.svg" "green.svg" "red.svg"))
(defconst picpocket-test-tree-files (list "green.svg"
                                     "cold/blue.svg"
                                     "warm/red.svg"))
(defvar picpocket-test-dir nil)
(defvar picpocket-delete-dir-after-test t)
(put 'picpocket-test-dir 'risky-local-variable t)

(defconst picpocket-expected-files
  (append (list "picpocket-db-journal.el"
                "picpocket-db-old.el"
                "picpocket-db.el"
                "picpocket-db-tmp.el")
          picpocket-test-files))

(defconst picpocket-expected-dirs
  (append (cl-loop for file in picpocket-test-tree-files
                   for dir = (file-name-directory file)
                   when dir
                   collect dir)
          (list "manga-dir/")))

(defvar picpocket-clock-file "~/share/elisp/picpocket-clock.txt")

(defun picpocket-add-tag-manga ())
(defun picpocket-add-tag-horror ())
(defvar picpocket-test-keystroke-alist '((?M add-tag "manga")
                                    (?H add-tag "horror")))


;;; Macros


(defmacro picpocket-with-test-dir (&rest body)
  "Run test in directory with blue.svg, green.svg and red.svg."
  (declare (debug (body))
           (indent defun))
  `(let* ((picpocket-test-dir (or picpocket-test-dir
                             (file-name-as-directory
                              (make-temp-file "picpocket-test-" t))))
          (picpocket-db-dir picpocket-test-dir)
          (picpocket-db nil)
          (picpocket-db-journal-size 0)
          (picpocket-inhibit-timers t)
          (picpocket-demote-warnings t)
          (picpocket-destination-relative-current t)
          (picpocket-recursive t)
          (picpocket-keystroke-alist 'picpocket-test-keystroke-alist)
          (picpocket-undo-ring nil)
          (picpocket-trashcan nil)
          (default-directory picpocket-test-dir))
     (get-buffer-create picpocket-undo-buffer)
     (make-directory picpocket-test-dir t)
     (unwind-protect
         (prog2
             (picpocket-ensure-only-test-files)
             (progn ,@body)
           (picpocket-check-only-expected-files))
       (kill-buffer picpocket-undo-buffer)
       (when picpocket-trashcan
         (delete-directory picpocket-trashcan t))
       (when picpocket-delete-dir-after-test
         (delete-directory picpocket-test-dir t)))))

(defmacro picpocket-with-test-buffer (&rest body)
  "Run picpocket test starting at blue.svg in directory with
blue.svg, green.svg and red.svg."
  (declare (debug (body))
           (indent defun))
  `(picpocket-with-test-dir
     (with-current-buffer (picpocket-directory default-directory "blue.svg")
       (prog1
           (progn ,@body)
         (when (buffer-live-p (get-buffer picpocket-buffer))
           (kill-buffer picpocket-buffer))))))


(defmacro picpocket-with-test-dir-tree (&rest body)
  "Run test in this tree:

green.svg
cold/blue.svg
warm/red.svg"
  (declare (debug (body))
           (indent defun))
  `(picpocket-with-test-dir
     (make-directory "cold")
     (rename-file "blue.svg" "cold")
     (make-directory "warm")
     (rename-file "red.svg" "warm")
     ,@body))

(defmacro picpocket-with-test-buffer-tree (&rest body)
  "Run picpocket test starting at green.svg in this tree:

green.svg
cold/blue.svg
warm/red.svg"
  (declare (debug (body))
           (indent defun))
  `(picpocket-with-test-dir-tree
     (with-current-buffer (picpocket-directory default-directory "green.svg")
       (prog1
           (progn ,@body)
         (kill-buffer picpocket-buffer)))))



(defmacro picpocket-report-time (&rest body)
  (declare (debug (body))
           (indent defun))
  `(cl-destructuring-bind (rc time)
       (picpocket-time ,@body)
     (message "Something in testcase %s took %s"
              (if ert--running-tests
                  (ert-test-name (car ert--running-tests))
                "none")
              (picpocket-sec-string time))
     rc))


;;; Benchmark

(defun picpocket-release-bench ()
  (let ((picpocket-clock-file "~/share/elisp/picpocket-clock-release.txt"))
    (picpocket-bench)))

(defun picpocket-tmp-bench ()
  (let ((picpocket-clock-file "~/share/elisp/picpocket-clock-tmp.txt"))
    (picpocket-bench))
  (with-current-buffer "*Messages*"
    (write-file "messages.txt")))

;; PENDING - picpocket-file-list bench
(defun picpocket-bench ()
  (let ((imagemagick-render-type 1))
    (picpocket-with-clock "Default benchmark"
      (picpocket-clock (picpocket-directory "~/bilder/japan/dvd/"))
      (with-current-buffer picpocket-buffer
        ;; (picpocket-toggle-fullscreen-frame)
        (dotimes (ignored 30)
          (picpocket-clock (picpocket-look-ahead-next))
          (picpocket-clock (redisplay))
          (picpocket-clock (picpocket-next)))))))

(defun picpocket-clock-report (title)
  (cl-loop for (thing . s) in picpocket-clock-alist
           do (message "%s %s" thing (picpocket-sec-string s)))
  (when picpocket-clock-file
    (with-current-buffer (let ((enable-local-variables :safe))
                           (find-file-noselect picpocket-clock-file))
      (goto-char (point-max))
      (insert "\n")
      (pp (append (picpocket-clock-info title)
                  picpocket-clock-alist)
          (current-buffer))
      (save-buffer))))

(defun picpocket-clock-info (title)
  (list (cons 'title title)
        (cons 'time (current-time))
        (cons 'cpu (picpocket-cpu))
        (cons 'linux (string-trim (shell-command-to-string
                                   "lsb_release -ds")))
        (cons 'emacs-version (cons emacs-version
                                   (emacs-repository-get-version)))
        (cons 'imagemagick-version (picpocket-imagemagick-version))
        (cons 'byte-compile (not (not (member ".elc" load-suffixes))))
        (cons 'render-type imagemagick-render-type)
        (cons 'picpocket-version picpocket-version)))

(defun picpocket-imagemagick-version ()
  (string-trim (cadr (split-string (shell-command-to-string
                                    "dpkg -s imagemagick | grep Version")
                                   " "))))

(defun picpocket-cpu ()
  (cadr (split-string (shell-command-to-string "lscpu  | grep 'Model name'")
                      ":" t "[\n\t ]+")))


(ert-deftest picpocket-test-picpocket-clock-thing ()
  (let (picpocket-clock-alist)
    (cl-flet ((picpocket-time (&rest _forms) (list 1 (seconds-to-time 1))))
      (picpocket-clock-thing :plus (+ 1 1))
      (picpocket-clock-thing :minus (- 1 1))
      (picpocket-clock-thing :plus (+ 1 1))
      (should (equal  (list (cons :minus (seconds-to-time 1))
                           (cons :plus (seconds-to-time 2)))
                     picpocket-clock-alist)))))

(ert-deftest picpocket-test-picpocket-clock ()
  (let (picpocket-clock-alist)
    (cl-flet ((picpocket-time (&rest _forms) (list 1 (seconds-to-time 1))))
      (picpocket-clock (+ 1 1))
      (picpocket-clock (- 1 1))
      (picpocket-clock (+ 1 1))
      (should (equal (list (cons '- (seconds-to-time 1))
                           (cons '+ (seconds-to-time 2)))
                     picpocket-clock-alist)))))

;; PENDING
;; Ignore the byte-compile field.  Maybe delete those old entries.
;; Field is-current-emacs.
;; Normally only is-current-emacs=t entries are shown.
;; Prefix arg shows all.

(defun picpocket-list-bench (keys byte-compiled)
  (interactive
   (list (mapcar #'intern (split-string (picpocket-bench-read-keys) nil t))
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
           (picpocket-pp (picpocket-filter-alist x keys)))
         (current-buffer))))))

(defun picpocket-pp (alist)
  (cl-loop for (key . value) in alist
           do (picpocket-pp2 key value)))

(defun picpocket-pp2 (key value)
  (insert (format "%-20s %s\n"
                  key
                  (cond ((and (listp value)
                              (eq 4 (safe-length value)))
                         (if (zerop (car value))
                             (format "%.2fs" (time-to-seconds value))
                           (current-time-string value)))
                        (t value)))))

(defun picpocket-filter-alist (alist keys)
  (if keys
      (cl-loop for entry in alist
               when (memq (car entry) keys)
               collect entry)
    alist))

(defun picpocket-bench-read-keys ()
  (picpocket-read-with-completion "Show these fields (RET for all): "
                             (picpocket-key-collection)))

(defun picpocket-read-with-completion (prompt
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
                           picpocket-minibuffer-map)))

(defun picpocket-key-collection ()
  (save-excursion
    (goto-char (point-min))
    (read (current-buffer))))



;;; Help functions

(defun picpocket-list-should-be-reset ()
  (should (eq picpocket-list nil))
  (should (eq picpocket-current nil))
  (should (eq picpocket-index 0))
  (should (eq picpocket-list-length 0)))


(defun picpocket-files-in-list ()
  (cl-loop for pic on picpocket-list
           collect (picpocket-relative-dir (concat (picpocket-dir pic)
                                              (picpocket-file pic)))))

(defun picpocket-relative-dir (dir)
  (substring dir (length (file-truename default-directory))))

(defun picpocket-ensure-only-test-files ()
  (when (file-exists-p picpocket-test-dir)
    (delete-directory picpocket-test-dir t))
  (make-directory picpocket-test-dir t)
  (let ((default-directory picpocket-test-dir))
    (dolist (file picpocket-test-files)
      (unless (file-exists-p file)
        (with-temp-file file
          (picpocket-insert-svg file))))))

(defun picpocket-insert-svg (file)
  (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n"
          "  <rect width=\"150\" height=\"150\" fill=\"rgb("
          (picpocket-svg-color file) ")\"\n"
          "   stroke-width=\"1\" stroke=\"rgb(0, 0, 0)\"/>\n"
          "</svg>\n"))

(defun picpocket-svg-color (file)
  (pcase (file-name-sans-extension file)
    ("red"   "255, 0, 0")
    ("green" "0, 255, 0")
    ("blue"  "0, 0, 255")))


(defun picpocket-check-only-expected-files ()
  (let ((default-directory picpocket-test-dir))
    (dolist (file (directory-files default-directory nil nil t))
      (unless (or (equal file ".")
                  (equal file ".."))
        (if (file-directory-p file)
            (unless (member (file-name-as-directory file)
                            picpocket-expected-dirs)
              (error "Unexpected directory %s in %s"
                     file default-directory))
          (unless (member file picpocket-expected-files)
            (error "Unexpected file %s in %s"
                   file default-directory)))))))

(defun picpocket-ensure-status-quo ()
  (should (eq 3 picpocket-list-length))
  (cl-loop for pic on picpocket-list
           for expected in picpocket-test-files
           do (should (equal (picpocket-file pic) expected)))
  (should (memq picpocket-index (list 1 2 3)))
  (should (memq (car picpocket-current) picpocket-list)))






;;; Test cases.

(ert-deftest picpocket-tree-add-tag-to-all ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer-tree
    (picpocket-report-time (picpocket-edit-tags 'all "manga"))
    (should (equal (picpocket-tags) '(manga)))
    (should (equal (picpocket-tags (cdr picpocket-current)) '(manga)))
    (should (equal (picpocket-tags (cddr picpocket-current)) '(manga)))))

(ert-deftest picpocket-tag-to-all ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-report-time (picpocket-edit-tags 'all "manga"))
    (should (equal (picpocket-tags) '(manga)))
    (should (equal (picpocket-tags (cdr picpocket-current)) '(manga)))
    (should (equal (picpocket-tags (cddr picpocket-current)) '(manga)))))

(ert-deftest picpocket-tree-move-within-tree ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer-tree
    (picpocket-report-time
      (picpocket-next)
      (picpocket-edit-tags nil "manga")
      (picpocket-rename "../warm"))
    (should (equal (picpocket-dir) (file-truename default-directory)))
    (should (equal (picpocket-file) "red.svg"))
    (should (file-exists-p "../warm/blue.svg"))))


(ert-deftest picpocket-tree-copy-within-tree ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer-tree
    (picpocket-report-time
      (picpocket-next)
      (picpocket-add-tag-manga)
      (picpocket-copy nil "../warm"))
    (should (equal 3 picpocket-list-length))
    (should (equal 3 (length picpocket-list)))
    (should (equal (picpocket-dir) (file-truename default-directory)))
    (should (equal (picpocket-file) "blue.svg"))
    (should (equal (picpocket-tags) '(manga)))
    (should (file-exists-p "blue.svg"))
    (picpocket-revert)
    (with-current-buffer picpocket-buffer
      (should (equal 4 picpocket-list-length)))))

(ert-deftest picpocket-tree-move-outside-tree ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer-tree
    (picpocket-report-time
      (picpocket-next)
      (picpocket-add-tag-manga)
      (let* ((outside (file-name-as-directory
                       (make-temp-file "picpocket-test-" t)))
             (outside-blue (concat outside "blue.svg")))
        (unwind-protect
            (progn
              (picpocket-move nil outside)
              (should (file-exists-p outside-blue)))
          (delete-file outside-blue)
          (delete-directory outside))))))

(ert-deftest picpocket-tree-add-and-clear-tag ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer-tree
    (picpocket-report-time
      (picpocket-next)
      (picpocket-add-tag-manga))
    (should (equal (picpocket-tags) '(manga)))
    (picpocket-report-time
      (picpocket-tags-delete-file picpocket-current (picpocket-absfile)))
    (should-not (picpocket-tags))))

(ert-deftest picpocket-tree-make-list-with-tags ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer-tree
    (picpocket-add-tag-manga)
    (picpocket-create-picpocket-list (picpocket-file-list ".")
                           (file-truename "green.svg"))
    (should (equal (picpocket-tags)
                   '(manga)))))


(ert-deftest picpocket-tree-make-list-with-tags2 ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer-tree
    (picpocket-add-tag-manga)
    (picpocket-next)
    (picpocket-add-tag-horror)
    (picpocket-report-time
      (picpocket-create-picpocket-list (picpocket-file-list "..")
                             (file-truename "../green.svg")))
    (should (equal (picpocket-tags)
                   '(manga)))
    (should (equal (picpocket-tags (cdr picpocket-current))
                   '(horror)))))

(ert-deftest picpocket-tree-file-list ()
  :tags '(:picpocket)
  (picpocket-with-test-dir-tree
    (should (equal (picpocket-report-time (picpocket-file-list
                                           default-directory))
                   (mapcar #'file-truename
                           picpocket-test-tree-files)))))

(ert-deftest picpocket-file-list-symlink-loop ()
  :tags '(:picpocket)
  (picpocket-with-test-dir-tree
    (unwind-protect (progn
                      (make-symbolic-link "." "self")
                      (make-symbolic-link "../warm" "cold/w")
                      (make-symbolic-link "../cold" "warm/c")
                      (should (eq 3 (length (picpocket-file-list
                                             picpocket-test-dir)))))
      (delete-directory picpocket-test-dir t)
      (make-directory picpocket-test-dir))))

(ert-deftest picpocket-file-list-dot-file ()
  :tags '(:picpocket)
  (picpocket-with-test-dir-tree
    (unwind-protect (progn
                      (make-directory ".dot")
                      (copy-file "green.svg" ".dot")
                      (should (eq 3 (length (picpocket-file-list
                                             picpocket-test-dir)))))
      (delete-directory (concat picpocket-test-dir ".dot") t))))


(ert-deftest picpocket-files-in-list-test ()
  :tags '(:picpocket)
  (picpocket-report-time
    (picpocket-with-test-buffer
      (should (equal picpocket-test-files
                     (picpocket-files-in-list)))
      (should (equal picpocket-list picpocket-current))
      (should (equal 3 picpocket-list-length))
      (should (equal 1 picpocket-index)))))

(ert-deftest picpocket-tree-files ()
  :tags '(:picpocket)
  (picpocket-report-time
    (picpocket-with-test-buffer-tree
      (should (equal picpocket-test-tree-files
                     (picpocket-files-in-list)))
      (should (eq picpocket-list picpocket-current))
      (should (equal 3 picpocket-list-length))
      (should (equal 1 picpocket-index)))))


(ert-deftest picpocket-insert-before-current ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    ;; Beginning
    (picpocket-list-delete)
    (picpocket-list-insert-before-current
     (picpocket-make-pic (file-truename "blue.svg")))
    (picpocket-check-only-expected-files)
    (picpocket-ensure-status-quo)
    ;; Middle
    (picpocket-home)
    (picpocket-next)
    (picpocket-list-delete)
    (picpocket-list-insert-before-current
     (picpocket-make-pic (file-truename "green.svg")))
    (picpocket-check-only-expected-files)
    (picpocket-ensure-status-quo)))

(ert-deftest picpocket-insert-after-current ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    ;; Middle
    (picpocket-home)
    (picpocket-next)
    (picpocket-list-delete)
    (picpocket-previous)
    (picpocket-list-insert-after-current
     (picpocket-make-pic (file-truename "green.svg")))
    (picpocket-check-only-expected-files)
    (picpocket-ensure-status-quo)
    ;; End
    (picpocket-end)
    (picpocket-list-delete)
    (picpocket-list-insert-after-current
     (picpocket-make-pic (file-truename "red.svg")))
    (picpocket-check-only-expected-files)
    (picpocket-ensure-status-quo)))

(ert-deftest picpocket-add-tag-delete-file ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-report-time
      (picpocket-add-tag-manga)
      (picpocket-delete-file))
    (should-not (file-exists-p "blue.svg"))
    (should-not (picpocket-tags))))


(ert-deftest picpocket-delete-from-beginning ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-delete-file)
    (should (eq 2 (length picpocket-list)))
    (picpocket-delete-file)
    (should (eq 1 (length picpocket-list)))
    (picpocket-delete-file)
    (picpocket-list-should-be-reset)))

(ert-deftest picpocket-delete-from-the-middle-and-end ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-next)
    (picpocket-delete-file)
    (should (eq 2 (length picpocket-list)))
    (picpocket-delete-file)
    (should (eq 1 (length picpocket-list)))
    (picpocket-delete-file)
    (picpocket-list-should-be-reset)))

(ert-deftest picpocket-move-all ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-move 'all "warm")
    (should (not picpocket-current))
    (should (eq (+ 2 3) (length (directory-files "warm"))))))

(ert-deftest picpocket-copy-all ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-next)
    (picpocket-copy 'all "warm")
    (should (eq 3 (length picpocket-list)))
    (should (eq (+ 2 3) (length (directory-files "warm"))))))

(ert-deftest picpocket-hardlink-all ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-end)
    (picpocket-hardlink 'all "warm")
    (should (eq 3 (length picpocket-list)))
    (should (eq (+ 2 3) (length (directory-files "warm"))))))


(defun picpocket-db-put-test-data ()
  (picpocket-db-init)
  (picpocket-db-put "1" "Some data")
  (picpocket-db-put "2" "Some more data")
  (picpocket-db-put "1" nil))

(defun picpocket-db-verify-test-data ()
  (should (equal "Some more data" (picpocket-db-get "2"))))




(defmacro picpocket-with-test-db (&rest body)
  (declare (debug (body))
           (indent defun))
  `(cl-loop for picpocket-db-format in picpocket-db-valid-formats
            collect (picpocket-with-test-dir
                      (picpocket-db-put-test-data)
                      ,@body)))

(ert-deftest picpocket-db-put-get-test ()
  (picpocket-with-test-db
    (picpocket-db-verify-test-data)))

(ert-deftest picpocket-db-read-database-file-test ()
  (picpocket-with-test-db
    (picpocket-db-save)
    (setq picpocket-db nil)
    (picpocket-db-init)
    (picpocket-db-verify-test-data)))

(ert-deftest picpocket-db-read-database-file-with-corrupt-fallback-test ()
  (picpocket-with-test-db
    (picpocket-db-save)
    (setq picpocket-db nil)
    (with-temp-file (picpocket-db-file :old)
      (insert "Typical corrupt data...\n"))
    (picpocket-db-init)
    (picpocket-db-verify-test-data)))

(ert-deftest picpocket-db-read-fallback-file-test ()
  (picpocket-with-test-db
    (picpocket-db-save)
    (setq picpocket-db nil)
    (rename-file (picpocket-db-file)
                 (picpocket-db-file :old))
    (picpocket-db-init)
    (picpocket-db-verify-test-data)))

(ert-deftest picpocket-db-read-fallback-instead-of-corrupt-database-file-test ()
  (picpocket-with-test-db
    (picpocket-db-save)
    (setq picpocket-db nil)
    (rename-file (picpocket-db-file)
                 (picpocket-db-file :old))
    (with-temp-file (picpocket-db-file)
      (insert "Typical corrupt data...\n"))
    (picpocket-db-init)
    (picpocket-db-verify-test-data)))

(ert-deftest picpocket-db-read-journal-test ()
  (picpocket-with-test-db
    (setq picpocket-db nil)
    (picpocket-db-init)
    (picpocket-db-verify-test-data)))


(ert-deftest picpocket-db-tags-test ()
  (cl-labels ((expect-files (&rest files)
                            (should (picpocket-lists-equal
                                     (picpocket-db-files "sha1")
                                     files)))
              (expect-tags (tags)
                           (should (picpocket-lists-equal
                                    (picpocket-db-tags "sha1")
                                    tags))))
    (picpocket-with-test-dir
      (picpocket-db-clear)
      (picpocket-db-tags-set "sha1" "a" '(cool fonzy))
      (picpocket-db-tags-add-file "sha1" "b")
      (expect-files "a" "b")

      (picpocket-db-tags-set "sha1" "c" '(something else))
      (expect-files "a" "b" "c")
      (expect-tags '(something else))

      (picpocket-db-tags-set "sha1" "a" nil)
      (expect-tags nil)
      (should (zerop (picpocket-db-count)))

      (picpocket-db-tags-set "sha1" "a" '(other stuff))
      (picpocket-db-tags-move-file "sha1" "a" "b")
      (expect-files "b")

      (picpocket-db-tags-add-file "sha1" "c")
      (picpocket-db-tags-move-file "sha1" "b" "c")
      (expect-files "c")

      (picpocket-db-tags-copy-file "sha1" "d")
      (expect-files "c" "d")

      (picpocket-db-tags-add-file "sha1" "e")
      (picpocket-db-tags-copy-file "sha1" "c")
      (expect-files "c" "d" "e")

      (picpocket-db-tags-delete-file "sha1" "c")
      (expect-files "d" "e")
      (picpocket-db-tags-delete-file "sha1" "d")
      (expect-files "e")
      (picpocket-db-tags-delete-file "sha1" "e")
      (expect-files)
      (expect-tags nil)
      (should (zerop (picpocket-db-count))))))

(defun picpocket-lists-equal (a b)
  (equal (sort (cl-copy-list a) 'picpocket-lessp)
         (sort (cl-copy-list b) 'picpocket-lessp)))

(defun picpocket-lessp (a b)
  (string-lessp (prin1-to-string a) (prin1-to-string b)))



(ert-deftest picpocket-external-file-sha-change ()
  (picpocket-with-test-buffer
    (picpocket-add-tag "dragon")
    (append-to-file " " nil "blue.svg")
    (let ((sha-changed (cdr (assq :sha-changed (picpocket-db-traverse))))
          (sha (picpocket-sha1sum "blue.svg")))
      (should (eq 1 (length sha-changed)))
      (picpocket-update-sha sha-changed)
      (should (picpocket-db-get sha))
      (should (equal (plist-get (picpocket-db-get sha) :tags)
                     '(dragon))))))

(ert-deftest picpocket-delete-unique-tagged-file ()
  (picpocket-with-test-buffer
    (picpocket-add-tag "troll")
    (delete-file "blue.svg")
    (let ((unique-file-missing (cdr (assq :unique-file-missing
                                          (picpocket-db-traverse)))))
      (should (eq 1 (length unique-file-missing)))
      (picpocket-remove-file-names-in-db unique-file-missing)
      (should (zerop (picpocket-db-count))))))

(defun picpocket-add-tag (tag-string)
  (picpocket-command
    (picpocket-action 'add-tag tag-string)))

(ert-deftest picpocket-delete-redundant-tagged-file ()
  (picpocket-with-test-buffer
    (copy-file "blue.svg" "green.svg" t)
    (picpocket-add-tag "troll")
    (cl-loop for pic on picpocket-list
             do (picpocket-sha-force pic))
    (picpocket-db-dump)
    (should (eq 1 (picpocket-db-count)))
    (delete-file "blue.svg")
    (let ((redundant-file-missing (cdr (assq :redundant-file-missing
                                             (picpocket-db-traverse)))))
      (should (eq 1 (length redundant-file-missing)))
      (picpocket-remove-file-names-in-db redundant-file-missing))
    (should (eq 1 (picpocket-db-count)))))


(ert-deftest picpocket-test-pic-by-index ()
  (picpocket-with-test-buffer
    (should-not (picpocket-pic-by-index 0))
    (should (equal "blue.svg" (picpocket-file (picpocket-pic-by-index 1))))
    (should (equal "red.svg" (picpocket-file (picpocket-pic-by-index 3))))
    (should-not (picpocket-pic-by-index 4))))

(ert-deftest picpocket-test-jump ()
  (picpocket-with-test-buffer-tree
    (picpocket-jump-to-file "green.svg")
    (should (equal 1 picpocket-index))
    (picpocket-rename "red.svg")
    (let ((reds (picpocket-pos-list-by-file "red.svg")))
      (should (eq 2 (length reds)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest ignored) (cadr coll))))
        (let ((pic (picpocket-select-pos-by-dir reds "Choose wisely: ")))
          (should (eq 3 (picpocket-calculate-index pic))))))))


(ert-deftest picpocket-test-compute-filter-match-count ()
  (picpocket-with-test-buffer
    (should-not picpocket-filter-match-count-done)
    (should-not picpocket-filter-match-count)
    ;; No filter.
    (picpocket-compute-filter-match-count #'ignore nil)
    (should-not picpocket-filter-match-count-done)
    (should-not picpocket-filter-match-count)
    ;; No matches.
    (picpocket-set-filter "orc")
    (picpocket-compute-filter-match-count #'ignore nil)
    (should picpocket-filter-match-count-done)
    (should (zerop picpocket-filter-match-count))
    ;; Two matches.
    (picpocket-set-filter "")
    (picpocket-edit-tags nil "green orc maniac")
    (picpocket-next)
    (picpocket-edit-tags nil "orc")
    (picpocket-previous)
    (picpocket-set-filter "orc")
    (picpocket-compute-filter-match-count #'ignore nil)
    (should picpocket-filter-match-count-done)
    (should (eq 2 picpocket-filter-match-count))
    ;; Force rescan.
    (picpocket-set-filter "orc")
    (picpocket-compute-filter-match-count #'ignore nil)
    (should picpocket-filter-match-count-done)
    (should (eq 2 picpocket-filter-match-count))
    ;; Alter tags.
    (picpocket-edit-tags nil "troll berserk")
    (should (eq 1 picpocket-filter-match-count))
    (picpocket-edit-tags nil "vampire duke")
    (should (zerop picpocket-filter-match-count))
    ;; Delete file.
    (picpocket-set-filter "")
    (picpocket-home)
    (picpocket-edit-tags nil "orc horde")
    (picpocket-next)
    (picpocket-edit-tags nil "orc minion")
    (picpocket-set-filter "orc")
    (picpocket-compute-filter-match-count #'ignore nil)
    (should (eq 2 picpocket-filter-match-count))
    (picpocket-delete-file)
    (should picpocket-filter-match-count-done)
    (should (eq 1 picpocket-filter-match-count))
    (picpocket-delete-file)
    (should picpocket-filter-match-count-done)
    (should (zerop picpocket-filter-match-count))))

(ert-deftest picpocket-test-compute-filter-index ()
  (picpocket-with-test-buffer
    (should-not picpocket-filter-index)
    (picpocket-set-filter "orc")
    ;; No matches.
    (picpocket-compute-filter-index #'ignore nil)
    (should (zerop picpocket-filter-index))
    ;; Two matches.
    (picpocket-set-filter "")
    (picpocket-edit-tags nil "green orc maniac")
    (picpocket-next)
    (picpocket-edit-tags nil "orc")
    (picpocket-home)
    (picpocket-set-filter "orc")
    (picpocket-compute-filter-index #'ignore nil)
    (should (eq 1 picpocket-filter-index))
    (picpocket-next)
    (should (eq 2 picpocket-filter-index))
    ;; Force rescan.
    (picpocket-set-filter "orc")
    (picpocket-compute-filter-index #'ignore nil)
    (should (eq 2 picpocket-filter-index))
    ;; Alter tags.
    (picpocket-edit-tags nil "troll berserk")
    (should-not picpocket-filter-index)
    (picpocket-compute-filter-index #'ignore nil)
    (should (eq 1 picpocket-filter-index))
    ;; Delete.
    (picpocket-set-filter "")
    (picpocket-home)
    (picpocket-edit-tags nil "orc horde")
    (picpocket-next)
    (picpocket-edit-tags nil "orc minion")
    (picpocket-set-filter "orc")
    (picpocket-compute-filter-index #'ignore nil)
    (should (eq 2 picpocket-filter-index))
    (picpocket-delete-file)
    (should-not picpocket-filter-index)
    (picpocket-compute-filter-index #'ignore nil)
    (should (eq 1 picpocket-filter-index))
    (picpocket-delete-file)
    (should-not picpocket-filter-index)
    (picpocket-compute-filter-index #'ignore nil)
    (should (zerop picpocket-filter-index))))

(ert-deftest picpocket-empty-undo-buffer-test ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-visit-undo-list))
  (picpocket-with-test-buffer
    (let ((picpocket-undo-ring (make-ring picpocket-undo-list-size)))
      (picpocket-visit-undo-list))))

(ert-deftest picpocket-undo-buffer-test ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-delete-file)
    (picpocket-delete-file)
    (picpocket-delete-file)
    (picpocket-visit-undo-list)))

(ert-deftest picpocket-file-name-lessp-test ()
  (cl-loop for (a b) in '(("" "")
                          ("" "x")
                          ("x" "")
                          ("x" "x")
                          ("xxx" "yyy"))
           do (should (eq (string-lessp a b)
                          (picpocket-file-name-lessp a b))))
  (cl-loop for (a b) in '(("2" "10")
                          ("1.2" "1.10")
                          ("x2" "x10"))
           do (should (picpocket-file-name-lessp a b))))

(ert-deftest picpocket-trash-file-test ()
  (let ((picpocket-trashcan (file-name-as-directory
                        (make-temp-file "/tmp/picpocket-trash-file-test" t))))
    (dotimes (_ 4)
      (with-temp-file (picpocket-trash-file "foo.png")))
    (dolist (f '("foo.png" "foo_2.png" "foo_3.png" "foo_4.png"))
      (delete-file (expand-file-name f picpocket-trashcan)))
    (delete-directory picpocket-trashcan)))


(ert-deftest picpocket-single-undelete-test ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-add-tag-manga)
    (picpocket-delete-file)
    (picpocket-undo)
    (should (eq 3 (length picpocket-list)))
    (should (eq 3 picpocket-list-length))
    (should (file-exists-p "blue.svg"))
    (should (equal (picpocket-tags) '(manga)))))


(ert-deftest picpocket-undelete-all-test ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-delete-file)
    (picpocket-delete-file)
    (picpocket-delete-file)
    (picpocket-undo)
    (picpocket-undo)
    (picpocket-undo)
    (should (eq 3 (length picpocket-list)))
    (should (eq 3 picpocket-list-length))
    (should (file-exists-p "blue.svg"))
    (should (file-exists-p "green.svg"))
    (should (file-exists-p "red.svg"))))

(ert-deftest picpocket-undo-add-tag-test ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-add-tag-manga)
    (picpocket-undo)
    (should-not (picpocket-tags))))

(ert-deftest picpocket-undo-remove-tag-test ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-add-tag-manga)
    (picpocket-edit-tags nil "")
    (picpocket-undo)
    (should (equal (picpocket-tags) '(manga)))))

(ert-deftest picpocket-undo-rename-test ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-rename "bluish.svg")
    (should (equal (picpocket-file) "bluish.svg"))
    (picpocket-undo)
    (should (equal (picpocket-file) "blue.svg"))))

(ert-deftest picpocket-undo-move-test ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-add-tag-manga)
    (picpocket-move nil "subdir")
    (should (file-exists-p "subdir/blue.svg"))
    (should (eq 2 (length picpocket-list)))
    (picpocket-undo)
    (should (equal (picpocket-file) "blue.svg"))
    (should (file-exists-p "blue.svg"))
    (should (eq 3 (length picpocket-list)))
    (should (equal (picpocket-tags) '(manga)))
    (delete-directory "subdir")))

(ert-deftest picpocket-undo-copy-test ()
  :tags '(:picpocket)
  (picpocket-with-test-buffer
    (picpocket-add-tag-manga)
    (make-directory "subdir")
    (with-temp-file "subdir/blue.svg"
      (insert "some important document"))
    (let ((noninteractive t))
      (picpocket-copy nil "subdir"))
    (should (picpocket-files-identical-p "blue.svg" "subdir/blue.svg"))
    (picpocket-undo)
    (should (equal (picpocket-file-content "subdir/blue.svg")
                   "some important document"))
    (should (equal (picpocket-tags) '(manga)))
    (delete-directory "subdir" t)))



(provide 'picpocket-test)

;;; picpocket-test.el ends here
