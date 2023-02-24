;;; astrolisp.el --- Lisp Toolings for USGS astrogeology software   -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Hibl, Anton

;; Author: Anton Hibl
;; URL: https://github.com/antonhibl/astrolisp
;; Keywords: convenience, astrogeology
;; Version: 0.1.0
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

;; This package is essentially a set of functions and toolings for working with
;; astrogeology software such as ISIS3 and ALE in Emacs more locally with Lisp
;; tools and functions for better debugging on errors using tools like gdb. It
;; also allows for larger integration of these tools with Emacs which in turn
;; allows the extensible platform to integrate in new ways into the software. 

;;; Code:

;;; Customization
(defgroup astrolisp nil
  "Use the openAI API."
  :prefix "astrolisp-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/antonhibl/astrolisp"))

(defun astrolisp-extract-label (filename)
  "Function to extract a label from a cub file and strip binary opcodes.
Argument FILENAME input cube file."
  ;; creates a temporary file and assigns it to the variable temp-file
  (let ((temp-file (make-temp-file "catlab-")))
    (with-temp-buffer ;; creates a temporary buffer
      (insert-file-contents filename) ;; inserts the contents of filename
      ;; writes the contents of the temp buffer to the temp file
      (write-region (point-min) (point-max) temp-file))
    ;; creates a buffer and assigns it to the current buffer
    (with-current-buffer (get-buffer-create "*catlab*")
      ;; moves the current position of the buffer to the end
      (goto-char (point-max))
      ;; inserts the output from filename
      (insert (format "Output from %s:\n\n" filename))
      ;; creates a variable old-point and assigns it the current position
      (let ((old-point (point)))
        ;; inserts the contents of the temp file into the buffer
        (insert (substring-no-properties
                 (with-temp-buffer
                   (insert-file-contents temp-file)
                   (buffer-string))))
        ;; moves the current position of the buffer to the old-point variable
        (goto-char old-point)
        ;; deletes all lines in the buffer that are binary symbols
        (delete-non-matching-lines "^[\x20-\x7E]+$")
        ;; moves the current position of the buffer to the end
        (goto-char (point-max)))
      ;; displays the current buffer
      (display-buffer (current-buffer))
      ;; activates conf-space-mode in the current buffer
      (conf-space-mode))
    ;; deletes the temp file
    (delete-file temp-file)))

;; Uses a custom implementation compared to the shell version. I do not save
;; directly to a file because it is trivial to save a buffer to a file once
;; generated and this conserves generating unwanted label files/\.
(defun astrolisp-catlab ()
  "Interactive function to call label extractor."
  (interactive)
  (let
      ;; the input cube file
      ((from-file
        (read-file-name "from: ")))
    ;; call label extractor with provided args
    (astrolisp-extract-label from-file)))

;; cathist
(defun astrolisp-cathist ()
  "Outputs the full or brief history of a .cub file."
  (interactive)
  (let (
        (from-file
         (read-file-name "from: "))
        (to-file
         (read-string "to :")))
    (async-shell-command
     (format "cathist from=%s to=%s" from-file to-file))))

;; spiceinit
(defun astrolisp-spiceinit ()
  "Function to call spiceinit on a image-cube file."
  (interactive)
  ;; the input cube file
  (let
      ((from-file
        (read-file-name  "from: ")))
    ;; run spiceinit with args
    (async-shell-command
     (format "spiceinit from=%s" from-file))))

;; clem2isis
(defun astrolisp-clem2isis ()
  "Call clem2isis with the provided args."
  (interactive)
  (let (
        ; the input Clementine image
        (from-file
         (read-file-name "from: "))
        ; the filename for the output cube
        (to-file
         (read-string "to: ")))
    ;; run clem2isis with args
    (async-shell-command
     (format "clem2isis from=%s to=%s" from-file to-file))))

;; pds2isis
(defun astrolisp-pds2isis ()
  "Call pds2isis with the provided args."
  (interactive)
  (let (
        ; the input PDS, PDS label, or ISIS2 file
        (from-file
         (read-file-name "from: "))
        ;; the detached image file
        (img-file
         (read-file-name "image: "))
        ; the filename for the output cube
        (to-file
         (read-string "to: ")))
    ;; run pds2isis with args
    (async-shell-command
     (format "pds2isis from=%s to=%s image=%s" from-file to-file img-file))))

;; tgocassis2isis
(defun astrolisp-tgocassis2isis ()
  "Call tgocassis2isis with the provided args."
  (interactive)
  (let (
        ; the PDS4 CaSSIS formatted XML file containing image metadata
        (from-file (read-file-name "from: "))
        ; the filename for the output cube
        (to-file (read-string "to: ")))
    ;; run tgocassis2isis with args
    (async-shell-command
     (format "tgocassiss2isis from=%s to=%s" from-file to-file))))

;; marci2isis
(defun astrolisp-marci2isis ()
  "Call marci2isis with the provided args."
  (interactive)
  (let (
        ; the input MRO MARCI image
        (from-file (read-file-name "from: "))
        ; the filename for the output cube
        (to-file (read-string "to: ")))
    ;; run marci2isis with args
    (async-shell-command
     (format "marci2isis from=%s to=%s" from-file to-file))))

;; thm2isis
(defun astrolisp-thm2isis ()
  "Call thm2isis with the provided args."
  (interactive)
  (let (
        ; the input PDS Themis EDR/RDR file
        (from-file (read-file-name "from: "))
        ; the filename for the output isis cube
        (to-file (read-string "to: ")))
    ;; run thm2isis with args
    (async-shell-command
     (format "thm2isis from=%s to=%s" from-file to-file))))

;; caminfo
;;
;; If the cube has no associated SPICE information, caminfo can run spiceinit to
;; generate it.  Output can be in PVL or CSV format, with the option to append new
;; information to the output file.  WKT format is used for POLYGON or USELABEL
;; options.  CSV format only allows Camstats, Statistics, and Geometry options. Isis
;; Label, Original Label, and Polygon options are disabled for CSV format.
(defun astrolisp-caminfo ()
"compiles & outputs the spacecraft & instrument data from a lvl-1 cube."
  (interactive)
  (let (
        ;; the input cube
        (from-file (read-file-name "from: "))
        ;; the output PVL file
        (to-file (read-string "to: ")))
    ;; run caminfo with args
    (async-shell-command
     (format "caminfo from=%s to=%s" from-file to-file))))

(provide 'astrolisp)
;;; astrolisp.el ends here
