; picpocket-test.el --- Some random code -*- lexical-binding: t -*-

;; Copyright (C) 2013 Johan Claesson
;; Author: Johan Claesson <johanclaesson@bredband.net>
;; Created:    <2013-03-03>
;; Time-stamp: <2015-04-26 17:27:09 jcl>
;; Version: 11

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
(require 'picpocket (concat default-directory "picpocket.el"))

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


;;; Macros

(defmacro picp-time (&rest body)
  (declare (debug ((symbolp form) body))
           (indent defun))
  `(cl-destructuring-bind (rc time-string)
       (picp-measure-time ,@body)
     (message "Something in testcase %s took %s"
              (if ert--running-tests
                  (ert-test-name (car ert--running-tests))
                "none")
              time-string)
     rc))

(defmacro picp-with-test-dir (&rest body)
  "Run test in directory with blue.svg, green.svg and red.svg."
  (declare (debug ((symbolp form) body))
           (indent defun))
  `(let* ((picp-test-dir (or picp-test-dir
                             (file-name-as-directory
                              (make-temp-file "picpocket-test-" t))))
          (picp-db-dir picp-test-dir)
          (picp-db nil)
          (picp-db-journal-size 0)
          (picp-inhibit-timers t)
          (picp-demote-warnings t)
          (picp-dst-dir-is-cwd t)
          (picp-recursive t)
          (default-directory picp-test-dir))
     (make-directory picp-test-dir t)
     (unwind-protect
         (prog2
             (picp-ensure-only-test-files)
             (progn ,@body)
           (picp-check-only-expected-files))
       (when picp-delete-dir-after-test
         (delete-directory picp-test-dir t)))))

(defmacro picp-with-test-buffer (&rest body)
  "Run picpocket test starting at blue.svg in directory with
blue.svg, green.svg and red.svg."
  (declare (debug ((symbolp form) body))
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
  (declare (debug ((symbolp form) body))
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
  (declare (debug ((symbolp form) body))
           (indent defun))
  `(picp-with-test-dir-tree
     (with-current-buffer (picpocket-dir default-directory "green.svg")
       (prog1
           (progn ,@body)
         (kill-buffer picp-buffer)))))


;;; Help functions.

(defun picp-should-be-reset ()
  (list (should (eq picp-list nil))
        (should (eq picp-current nil))
        (should (eq picp-index 0))
        (should (eq picp-length 0))))

(defun picp-dump ()
  (interactive)
  (picp-print 'picp-list)
  (picp-print 'picp-current)
  (picp-print 'picp-index)
  (picp-print 'picp-length))

(defun picp-print (var)
  (let ((value (symbol-value var)))
    (message "%-20s%s"
             var
             (with-temp-buffer
               (pp value (current-buffer))
               (goto-char (point-min))
               (forward-line)
               (indent-rigidly (point) (point-max) 16)
               (buffer-string)))))

(defun picp-files ()
  (cl-loop for pic on picp-list
           collect (picp-relative-dir (concat (picp-dir pic)
                                              (picp-file pic)))))

(defun picp-relative-dir (dir)
  (substring dir (length (file-truename default-directory))))

(defun picp-ensure-only-test-files ()
  (let ((default-directory picp-test-dir))
    (dolist (file (directory-files default-directory nil nil t))
      (unless (or (equal file ".")
                  (equal file "..")
                  (member file picp-test-files))
        (if (file-directory-p file)
            (delete-directory file t)
          (delete-file file))))
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
    (list (picp-time (picp-tag-to-all "manga"))
          (should (equal (picp-tags) '(manga)))
          (should (equal (picp-tags (cdr picp-current)) '(manga)))
          (should (equal (picp-tags (cddr picp-current)) '(manga))))))

(ert-deftest picp-tag-to-all ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (list (picp-time (picp-tag-to-all "manga"))
          (should (equal (picp-tags) '(manga)))
          (should (equal (picp-tags (cdr picp-current)) '(manga)))
          (should (equal (picp-tags (cddr picp-current)) '(manga))))))

(ert-deftest picp-tree-move-within-tree ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (list (picp-time
            (picp-next)
            (picp-add-tag "manga")
            (picp-rename "../warm"))
          (should (equal (picp-dir) (file-truename default-directory)))
          (should (equal (picp-file) "red.svg"))
          (should (file-exists-p "../warm/blue.svg")))))


(ert-deftest picp-tree-copy-within-tree ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (list (picp-time
            (picp-next)
            (picp-add-tag "manga")
            (picp-action 'copy "../warm"))
          (should (equal 3 picp-length))
          (should (equal 3 (length picp-list)))
          (should (equal (picp-dir) (file-truename default-directory)))
          (should (equal (picp-file) "blue.svg"))
          (should (equal (picp-tags) '(manga)))
          (should (file-exists-p "blue.svg"))
          (with-current-buffer (picp-revert)
            (should (equal 4 picp-length))))))

(ert-deftest picp-tree-move-outside-tree ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (picp-time
      (picp-next)
      (picp-add-tag "manga")
      (let* ((outside (file-name-as-directory (make-temp-file "picp-test-" t)))
             (outside-blue (concat outside "blue.svg")))
        (unwind-protect
            (list (picp-action 'move outside)
                  (should (file-exists-p outside-blue)))
          (delete-file outside-blue)
          (delete-directory outside))))))

(ert-deftest picp-tree-add-and-clear-tag ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (list (picp-time
            (picp-next)
            (picp-add-tag "manga"))
          (should (equal (picp-tags) '(manga)))
          (picp-time
            (picp-tags-delete-file picp-current (picp-path)))
          (should-not (picp-tags)))))


(ert-deftest picp-tree-make-list-with-tags ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (list (picp-add-tag "manga")
          (picp-create-picp-list (picp-file-list ".")
                                 (file-truename "green.svg"))
          (should (equal (picp-tags)
                         '(manga))))))


(ert-deftest picp-tree-make-list-with-tags2 ()
  :tags '(:picpocket)
  (picp-with-test-buffer-tree
    (list (picp-add-tag "manga")
          (picp-next)
          (picp-add-tag "horror")
          (picp-time
            (picp-create-picp-list (picp-file-list "..")
                                   (file-truename "../green.svg")))
          (should (equal (picp-tags)
                         '(manga)))
          (should (equal (picp-tags (cdr picp-current))
                         '(horror))))))

(ert-deftest picp-tree-file-list ()
  :tags '(:picpocket)
  (picp-with-test-dir-tree
    (should (equal (picp-time (picp-file-list default-directory))
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
                      (should (eq 3 (length (picp-file-list picp-test-dir)))))
      (delete-directory (concat picp-test-dir ".dot") t))))


(ert-deftest picp-files-test ()
  :tags '(:picpocket)
  (picp-time
    (picp-with-test-buffer
      (list (should (equal picp-test-files
                           (picp-files)))
            (should (equal picp-list picp-current))
            (should (equal 3 picp-length))
            (should (equal 1 picp-index))))))

(ert-deftest picp-tree-files ()
  :tags '(:picpocket)
  (picp-time
    (picp-with-test-buffer-tree
      (list (should (equal picp-test-tree-files
                           (picp-files)))
            (should (eq picp-list picp-current))
            (should (equal 3 picp-length))
            (should (equal 1 picp-index))))))


(ert-deftest picp-add-tag-delete-file ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (list (picp-time
            (picp-add-tag "manga")
            (picp-delete)
            (picp-update-buffer))
          (should-not (file-exists-p "blue.svg"))
          (should-not (picp-tags)))))


(ert-deftest picp-delete-from-beginning ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (list (picp-dump)
          (picp-delete)
          (picp-dump)
          (should (eq 2 (length picp-list)))
          (picp-delete)
          (picp-dump)
          (should (eq 1 (length picp-list)))
          (picp-delete)
          (picp-update-buffer)
          (picp-dump)
          (picp-should-be-reset))))

(ert-deftest picp-delete-from-the-middle-and-end ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (list (picp-next)
          (picp-delete)
          (should (eq 2 (length picp-list)))
          (picp-delete)
          (should (eq 1 (length picp-list)))
          (picp-delete)
          (picp-update-buffer)
          (picp-dump)
          (picp-should-be-reset))))

(ert-deftest picp-move-all ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (list (picp-move-all "warm")
          (should (not picp-current))
          (should (eq (+ 2 3) (length (directory-files "warm")))))))

(ert-deftest picp-copy-all ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (list (picp-next)
          (picp-copy-all "warm")
          (should (eq 3 (length picp-list)))
          (should (eq (+ 2 3) (length (directory-files "warm")))))))

(ert-deftest picp-hard-link-all ()
  :tags '(:picpocket)
  (picp-with-test-buffer
    (list (picp-end)
          (picp-hard-link-all "warm")
          (should (eq 3 (length picp-list)))
          (should (eq (+ 2 3) (length (directory-files "warm")))))))


(defun picp-db-put-test-data ()
  (picp-db-init)
  (picp-db-put "1" "Some data")
  (picp-db-put "2" "Some more data")
  (picp-db-put "1" nil))

(defun picp-db-verify-test-data ()
  (should (equal "Some more data" (picp-db-get "2"))))




(defmacro picp-with-test-db (&rest body)
  (declare (debug ((symbolp form) body))
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
      (list (picp-db-clear)

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
            (should (zerop (picp-db-count)))))))


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
    (let ((unique-file-missing (cdr (assq :unique-file-missing (picp-db-traverse)))))
      (should (eq 1 (length unique-file-missing)))
      (picp-remove-file-names-in-db unique-file-missing)
      (should (zerop (picp-db-count))))))


(ert-deftest picp-delete-redundant-tagged-file ()
  (picp-with-test-buffer
    (copy-file "blue.svg" "green.svg" t)
    (picp-add-tag "troll")
    (picp-compute-sha #'ignore nil)
    (picp-db-dump)
    (should (eq 1 (picp-db-count)))
    (delete-file "blue.svg")
    (let ((redundant-file-missing (cdr (assq :redundant-file-missing (picp-db-traverse)))))
      (should (eq 1 (length redundant-file-missing)))
      (picp-remove-file-names-in-db redundant-file-missing))
    (should (eq 1 (picp-db-count)))))


(provide 'picpocket-test)

;;; picpocket-test.el ends here

