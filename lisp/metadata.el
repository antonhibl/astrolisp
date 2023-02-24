(defun astrolisp-display-metadata (filename) 
  "Read and display the metadata of
an ISIS cube file." 
  (interactive "fEnter file name: ") 
  (let ((metadata (with-temp-buffer (insert-file-contents-literally filename) 
                                    (goto-char (point-min)) 
                                    (search-forward "Object = QUBE") 
                                    (forward-line) 
                                    (let ((start (point))) 
                                      (search-forward "End_Object") 
                                      (buffer-substring 
                                       start
                                       (point)))))) 
    (with-output-to-temp-buffer "*isis-metadata*" (princ metadata))))
