;;; run-tests --- run local tests in a script file, because Windows
;;; Commentary:
;;; - because none of the tooling to install dependencies, etc is
;;;   present for Windows
;;; Code:
(progn
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)
  (package-refresh-contents)
  (mapc 'package-install '(pythonic dash s f ert-runner))
  (load-file "conda.el")
  (load-file "test/conda-test.el")
  (ert-run-tests-batch-and-exit))

(provide 'run-tests)
;;; run-tests ends here
