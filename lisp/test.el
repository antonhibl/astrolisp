(defun astrolisp-isis-ctests (regex)
  "Run ISIS3 ctests, optionally pass a regex to specify which tests to run.
Argument REGEX regex pattern to specify which tests to run, defaults to all."
  (interactive
   (list ((read-string "ctests regex: " "\\."))))
  (let ((ctest-cmd "ctest -VV -R"))
    ;; change the directory to $ISISROOT
    (async-shell-command (format "cd $ISISROOT;%s \"%s\"" ctest-cmd regex)))
  )

(astrolisp-isis-ctests "\\TgoCassisstitch.")
