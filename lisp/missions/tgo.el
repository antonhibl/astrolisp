;; tgocassis2isis
(defun astrolisp-tgocassis2isis ()
  "Call tgocassis2isis with the provided args."
  (interactive)
  (let (
        ; the PDS4 CaSSIS formatted XML file containing image metadata
        (from-file (read-file-name "from: "))
        ; the filename for the output cube
        (to-file (read-file-name "to: ")))
    ;; run tgocassis2isis with args
    (async-shell-command
     (format "tgocassiss2isis from=%s to=%s" from-file to-file))))

;; tgocassisstitch
(defun astrolisp-tgocassisstitch ()
  "Call tgocassisstitch with the provided args"
  (interactive)
  (let (
        ;; from list of PDS4 CaSSIS cube files
        (from-list
         (read-file-name "fromlist: "))
        (out
         (read-string "out: "))
        (suffix-flag
         (or (read-string "suffix: ") "false")))
    (shell-command (format "tgocassisstitch fromlist=%s out=%s suffix=%s"
                       (shell-quote-argument from-list)
                       (shell-quote-argument out)
                       suffix-flag))))
