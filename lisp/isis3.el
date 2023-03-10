(defun astrolisp-isis3-cmake (&optional tests-p)
  "run the cmake command to build ISIS3.
Argument TESTS-P flag to signal whether to run cmake with tests, defaults to OFF."
  (interactive)
  (setq tests-p (if (equal tests-p "ON") "ON" "OFF"))
  (let ((cmake-cmd (format "cmake -DJP2KFLAG=OFF -GNinja -DbuildTests=%s" tests-p)))
    (async-shell-command (format ("cd $ISISROOT;%s ../isis" cmake-cmd)))))

(defun astrolisp-isis3-ninja ()
  "Run the ninja command to install ISIS3 components."
  (interactive)
  (let ((ninja-cmd "ninja"))
    (async-shell-command (format "cd $ISISROOT;%s" ninja-cmd))))
