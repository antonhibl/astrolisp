;; csminit
(defun astrolisp-csminit ()
  "Calls spiceinit on a image-cube file."
  (interactive)
  (let
      ;; the input cube file
      ((from
        (read-file-name  "from: "))
       (state
        (read-string "csm state: "))
       (isd
        (read-file-name "ISD: ")))
    ;; run spiceinit with args
    (async-shell-command
     (format "csminit from=%s isd=%S state=%s" from isd state))))
