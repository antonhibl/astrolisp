(defun astrolisp-isis3-ctests (regex)
  "Run ISIS3 ctests, optionally pass a regex to specify which tests to run.
Argument REGEX regex pattern to specify which tests to run, defaults to all."
  (interactive
   (list (read-string "ctests regex: " "\\.")))
  (let ((ctest-cmd "ctest -VV -R"))
    ;; change the directory to $ISISROOT
    (async-shell-command (format "cd $ISISROOT;%s \"%s\"" ctest-cmd regex)))
  )

(defun astrolisp-ale-pytests (test)
  "Run ALE pytests, optionally name a specific test file.
Argument TEST test file to run, defaults to * for all tests."
  (interactive
   (list (read-string "Test: " "*")))
  (async-shell-command (format "cd /Users/ahibl/Projects/Ale/;pytest
tests/pytests/%s" test)))

(defun astrolisp-ale-ctests (regex)
  "Run Ale ctests, optionally pass a regex to specify which tests to run.
Argument REGEX regex pattern to specify which tests to run, defaults to all."
  (interactive(list (read-string "ctests regex: " "\\.")))
  (let ((ctest-cmd "ctest -VV -R"))
    ;; change the directory to $ALEROOT
    (async-shell-command (format "cd /Users/ahibl/Projects/Ale;%s \"%s\"" ctest-cmd regex)))
  )
