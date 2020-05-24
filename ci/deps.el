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
  (package-install-file "conda.el"))
  

(provide 'deps)
;;; deps ends here
