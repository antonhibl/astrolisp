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
         (read-file-name "to: ")))
    ;; run clem2isis with args
    (async-shell-command
     (format "clem2isis from=%s to=%s" from-file to-file))))
