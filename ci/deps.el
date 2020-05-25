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
  (mapc 'package-install '(pythonic dash s f)))

  

(provide 'deps)
;;; deps ends here
