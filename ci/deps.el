;;; deps --- Install `conda.el' and its dependencies in CI
;;; Commentary:
;;; - because none of the tooling to install dependencies, etc is
;;;   present for Windows
;;; Code:
(progn
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)
  (package-refresh-contents)
  ;; TODO: restore `(package-install-file "conda.el")`, which currently
  ;; falls down on Windows with a "Package is missing Version header" error
  (mapc 'package-install '(pythonic dash s f)))



(provide 'deps)
;;; deps ends here
