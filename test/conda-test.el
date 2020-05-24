;;; conda-test --- Tests for conda.el
;;; Commentary:
;;; Code:
(require 'ert)
(require 'cl)
(require 'f)

;; Here we pull in the actual code under test
(require 'conda (f-expand "conda.el" (f-parent (f-dirname (f-this-file)))))

;; Rudimentary test to get us going
(ert-deftest test-conda-env-candidates ()
  (setq conda-anaconda-home "/usr/share/miniconda3")
  (setq conda-env-home-directory "/usr/share/miniconda3")
  (should
   (equal
    (conda-env-candidates)
    '("foo"))))

;; Not sure how to meaningfully test the below

;; (ert-deftest test-conda--set-env-gud-pdb-command-name)
;; (ert-deftest test-conda--set-system-gud-pdb-command-name)

;; (ert-deftest test-conda--filter-blanks ()
;;   (should (equal (conda--filter-blanks '("a" "" "c")) '("a" "c"))))

;; (ert-deftest test-conda--env-dir-is-valid ()
;;   (should (equal (conda--env-dir-is-valid "") nil))
;;   (cl-letf (((symbol-function 'f-directory?)
;;              (lambda (path)
;;                (equal path (concat "/home/user/sample/" conda-env-executables-dir)))))
;;     (should (equal (conda--env-dir-is-valid "/home/user/sample") t)))
;;   (cl-letf (((symbol-function 'file-exists-p)
;;              (lambda (path) nil)))
;;     (should (equal (conda--env-dir-is-valid "/home/user/sample") nil))))

;; (ert-deftest test-conda--purge-history ()
;;   (let ((conda-env-history '("one" "two" "three" "four" "/home/user/sample")))
;;     (conda--purge-history '("one" "four" "/home/user/sample"))
;;     (should (equal conda-env-history '("one" "four" "/home/user/sample"))))
;;   (let ((conda-env-history '("one" "two" "three" "four" "/home/user/sample")))
;;     (conda--purge-history '("one" "two" "/home/user/another-sample"))
;;     (should (equal conda-env-history '("one" "two")))))

;; (ert-deftest test-conda--read-env-name ()
;;   (defvar test-conda-read-env-name-prompt nil)
;;   (cl-letf (((symbol-function 'conda-env-read-name)
;;              (lambda (prompt)
;;                (setq test-conda-read-env-name-prompt prompt))))
;;     (let ((conda-env-current-name "sample"))
;;       (conda--read-env-name)
;;       (should (equal test-conda-read-env-name-prompt "Choose a conda environment (currently sample): ")))
;;     (let ((conda-env-current-name nil))
;;       (conda--read-env-name)
;;       (should (equal test-conda-read-env-name-prompt "Choose a conda environment: ")))))

;; TODO: How to test this meaningfully? Don't know that there's a good way, or
;; that it's really needed...
;; This assumes conda is available and installed already -- which should be OK
;; (ert-deftest test-conda--check-executable ()
;;   (let ((orig-exec-path exec-path))
;;     (while (executable-find "conda")
;;       (let ((conda-dir (file-name-directory (executable-find "conda"))))
;;         (setq exec-path (remove conda-dir exec-path))))
;;     (should-error (conda--check-executable))
;;     (setq exec-path orig-exec-path)
;;     (should (equal (conda--check-executable) nil))))

;; (ert-deftest test-conda--contains-env-yml-p ()
;;   (make-directory "/tmp/emacs-tests/" t)
;;   (write-region "" nil "/tmp/emacs-tests/environment.yml" t)
;;   (should (equal (conda--contains-env-yml? "/tmp/emacs-tests") t))
;;   (delete-file "/tmp/emacs-tests/environment.yml")
;;   (should (equal (conda--contains-env-yml? "/tmp/emacs-tests") nil)))

;; TODO: this fails on conda--contains-env-yml? for some reason
;; (ert-deftest test-conda--find-env-yml ()
;;   (make-directory "/tmp/emacs-tests/funky-sauce/things" t)
;;   (write-region "" nil "/tmp/emacs-tests/environment.yml" t)
;;   (should (equal (conda--find-env-yml "/tmp/emacs-tests/funky-sauce/things")
;;                  "/tmp/emacs-tests/environment.yml")))

;; (ert-deftest test-conda--get-name-from-env-yml ()
;;   (make-directory "/tmp/emacs-tests/" t)
;;   (write-region "name: Diggity" nil "/tmp/emacs-tests/environment.yml" t)
;;   (should (equal (conda--get-name-from-env-yml "/tmp/emacs-tests/environment.yml")
;;                  "Diggity")))

;; TODO: how to open a buffer under the /tmp/emacs-tests/ directory?
;; (ert-deftest test-conda--infer-env-from-buffer ()
;;    (should (= (+ 1 1) 1)))

;; (ert-deftest test-conda--set-python-shell-virtualenv-var ()
;;   (conda--set-python-shell-virtualenv-var "/tmp/emacs-tests/")
;;   (if (boundp 'python-shell-virtualenv-root)
;;       (should (equal python-shell-virtualenv-root "/tmp/emacs-tests/"))
;;     (should (equal python-shell-virtualenv-path "/tmp/emacs-tests/"))))

;; "public" functions

;; (ert-deftest test-conda-env-clear-history ()
;;    (should (= (+ 1 1) 1)))

;; (ert-deftest test-conda-env-default-location ()
;;    (should (= (+ 1 1) 1)))

;; (ert-deftest test-conda-env-name-to-dir ()
;;   ;; conda-env-default-location
;;   ;; conda-env-dir-is-valid for dirs under the default location
;;   (cl-letf (((symbol-function 'conda-env-default-location)
;;              (lambda ()
;;                "/home/user/envs"))
;;             (conda-env-executables-dir "bin")
;;             ((symbol-function 'f-directory?)
;;              (lambda (dir)
;;                (equal dir "/home/user/envs/sample/bin"))))
;;    (should (equal (conda-env-name-to-dir "sample") "/home/user/envs/sample"))))

;; (ert-deftest test-conda-env-dir-to-name ()
;;    (should (= (+ 1 1) 1)))

;; (ert-deftest test-conda-env-candidates ()
;;    (should (= (+ 1 1) 1)))

;; (ert-deftest test-conda-env-candidates-from-dir ()
;;    (should (= (+ 1 1) 1)))

;; (ert-deftest test-conda-env-stripped-path ()
;;   (cl-letf (((symbol-function 'conda-env-default-location)
;;              (lambda ()
;; 	       "/home/user/sample"))
;; 	    (conda-env-executables-dir "bin")
;; 	    (conda-env-current-name "env-name"))
;;     (should (equal (conda-env-stripped-path
;;                     "/usr/bin:/usr/local/bin:/home/user/sample/env-name/bin:/home/user/anaconda/bin")
;;                    (list "/usr/bin" "/usr/local/bin" "/home/user/anaconda/bin")))
;;     (should (equal (conda-env-stripped-path
;;                     "/usr/bin:/usr/local/bin:/home/user/anaconda/bin")
;;                    (list "/usr/bin" "/usr/local/bin" "/home/user/anaconda/bin")))))

;; (ert-deftest test-conda-env-is-valid ()
;;    (should (= (+ 1 1) 1)))

;; (ert-deftest test-conda-env-read-name ()
;;   ;; TODO: test that it passes completing-read some sensible candidates, and
;;   ;; that the `default` that's passed through is sensible too
;;   (defvar test-conda-env-read-name-args nil)
;;   (cl-letf (((symbol-function 'completing-read)
;;              (lambda (prompt coll &optional pred req hist default iim)
;;                (setq test-conda-env-read-name-args (list prompt coll pred req hist default iim)))))
;;    (cl-letf (((symbol-function 'conda-env-candidates)
;;               (lambda ()
;;                 '("one" "two" "three")))
;;              (conda-env-history (list "three" "two" "one")))
;;      (conda-env-read-name "prompt")
;;      (should (equal test-conda-env-read-name-args
;;                     '("prompt" ("one" "two" "three") nil t nil conda-env-history "three"))))
;;    (cl-letf (((symbol-function 'conda-env-candidates)
;;               (lambda ()
;;                 '("one" "two" "three")))
;;              (conda-env-history (list)))
;;      (conda-env-read-name "prompt")
;;      (should (equal test-conda-env-read-name-args
;;                     '("prompt" ("one" "two" "three") nil t nil conda-env-history "one"))))
;;   ))

;; potentially interactive user-exposed functions

;;;;###autoload
;; (ert-deftest test-conda-env-deactivate ()
;;    (should (= (+ 1 1) 1)))

;; ;;;;###autoload
;; (ert-deftest test-conda-env-activate ()
;;    (should (= (+ 1 1) 1)))

;; ;;;;###autoload
;; (ert-deftest test-conda-env-initialize-eshell ()
;;    (should (= (+ 1 1) 1)))

;; ;;;;###autoload
;; (ert-deftest test-conda-env-activate-for-buffer ()
;;    (should (= (+ 1 1) 1)))

;; (ert-deftest test-conda--switch-buffer-auto-activate ()
;;    (should (= (+ 1 1) 1)))
(provide 'conda-test)
;;; conda-test ends here
