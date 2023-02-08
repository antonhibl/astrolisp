;;; IsisTools.el --- Tool Suite for ISIS3 and ALE  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Anton Hibl
;; URL: https://github.com/antonhibl/Isis3Tools
;; Keywords: convenience, astrogeology
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.2"))

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

;; This is a global set of functions and tools for working with ISIS3 and ALE in
;; Emacs more locally with lisp tools and functions for better debugging on
;; errors. It also allows for larger integration of these tools with emacs which
;; is largely a worthwhile effort as a highly extensible platform for software.
;;
;; One tool is `catlab', this is a program to extract labels from .cub files and
;; astronomy data images used with the programs ISIS3 and ALE. This is a custom
;; implementation based on the original tooling found in the ISIS3 package
;; written in C++. Most tools will either be custom implementations or some
;; variation of the original shell command being piped through emacs.
;;
;; The various other toolings are listed below
;;

;;; Code:


(defun extract-label (filename)
  "Function to extract a label from a cub file and strip binary opcodes"
  (let ((temp-file (make-temp-file "catlab-")))
    (with-temp-buffer
      (insert-file-contents filename)
      (write-region (point-min) (point-max) temp-file))
    (with-current-buffer (get-buffer-create "*catlab*")
      (goto-char (point-max))
      (insert (format "Output from %s:\n\n" filename))
      (let ((old-point (point)))
        (insert (substring-no-properties
                 (with-temp-buffer
                   (insert-file-contents temp-file)
                   (buffer-string))))
        (goto-char old-point)
        (delete-non-matching-lines "^[\x20-\x7E]+$")
        (goto-char (point-max)))
      (display-buffer (current-buffer))
      (conf-space-mode))
    (delete-file temp-file)))

(defun catlab ()
  "Interactive function to call label extractor

Uses a custom implementation compared the shell version"
  (interactive)
  (let ((filename (read-file-name "Label file: ")))
    (extract-label filename)))

;; keybinds
(global-set-key (kbd "C-c l") 'catlab)

;;; IsisTools.el ends here
