(defun astrolisp-extract-pixels (file-path) 
  "Extracts the pixel values from an
ISIS cube file and returns them as a list."
  (let* ((buffer (find-file-noselect
                  file-path)) 
         (pixel-buffer (isis-cube-get-pixels buffer))) 
    (with-current-buffer pixel-buffer (goto-char (point-min)) 
                         (let ((result '())) 
                           (while (not (eobp)) 
                             (push (read (current-buffer)) result) 
                             (forward-line)) 
                           (reverse result)))))

(defun astrolisp-generate-statistics (file-path) 
  "Generates statistics for the pixel
values in an ISIS cube file."
  (let ((pixel-values (extract-pixel-values
                       file-path))) 
    `((minimum . ,(apply #'min pixel-values)) 
      (maximum . ,(apply #'max pixel-values)) 
      (mean . ,(apply #'mean pixel-values)) 
      (standard-deviation . ,(apply #'standard-deviation pixel-values)))))
