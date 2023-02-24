;; spiceinit
(defun astrolisp-spiceinit ()
  "Calls spiceinit on a image-cube file."
  (interactive)
  (let
      ;; the input cube file
      ((from-file
        (read-file-name  "from: ")))
    ;; run spiceinit with args
    (async-shell-command
     (format "spiceinit from=%s" from-file))))

;; spicefit
;; requires spice initialization
(defun astrolisp-spicefit ()
  "Uses least squares to fit spacecraft pointing data to parabola."
  (interactive)
  (let (
        ;; the cube to update
        (from-file
         (read-file-name "from: ")))
    ;; run spicefit with arg
    (async-shell-command
     (format "spicefit from=%s" from-file))))
