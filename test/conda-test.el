(require 'ert)
(require 'cl)
(require 'f)

;; Here we pull in the actual code under test
(require 'conda (f-expand "conda.el" (f-parent (f-dirname (f-this-file)))))

;; Not sure how to meaningfully test the below

; (ert-deftest test-conda--set-env-gud-pdb-command-name)
; (ert-deftest test-conda--set-system-gud-pdb-command-name)

(ert-deftest test-conda--filter-blanks ()
  (should (equal (conda--filter-blanks '("a" "" "c")) '("a" "c"))))

(ert-deftest test-conda--env-dir-is-valid ()
  (should (equal (conda--env-dir-is-valid "") nil))
  (cl-letf (((symbol-function 'f-directory?)
             (lambda (path)
               (equal path (concat "/home/user/sample/" conda-env-executables-dir)))))
    (should (equal (conda--env-dir-is-valid "/home/user/sample") t)))
  (cl-letf (((symbol-function 'file-exists-p)
             (lambda (path) nil)))
    (should (equal (conda--env-dir-is-valid "/home/user/sample") nil))))

(ert-deftest test-conda--purge-history ()
  (let ((conda-env-history '("one" "two" "three" "four" "/home/user/sample")))
    (conda--purge-history '("one" "four" "/home/user/sample"))
    (should (equal conda-env-history '("one" "four" "/home/user/sample"))))
  (let ((conda-env-history '("one" "two" "three" "four" "/home/user/sample")))
    (conda--purge-history '("one" "two" "/home/user/another-sample"))
    (should (equal conda-env-history '("one" "two")))))

(ert-deftest test-conda--read-env-name ()
  (defvar test-conda-read-env-name-prompt nil)
  (cl-letf (((symbol-function 'conda-env-read-name)
             (lambda (prompt)
               (setq test-conda-read-env-name-prompt prompt))))
    (let ((conda-env-current-name "sample"))
      (conda--read-env-name)
      (should (equal test-conda-read-env-name-prompt "Choose a conda environment (currently sample): ")))
    (let ((conda-env-current-name nil))
      (conda--read-env-name)
      (should (equal test-conda-read-env-name-prompt "Choose a conda environment: ")))))

(ert-deftest test-conda--check-executable ()
   (should (= (+ 1 1) 1)))

(ert-deftest test-conda--contains-env-yml-p ()
   (should (= (+ 1 1) 1)))

(ert-deftest test-conda--find-env-yml ()
   (should (= (+ 1 1) 1)))

(ert-deftest test-conda--get-name-from-env-yml ()
   (should (= (+ 1 1) 1)))

(ert-deftest test-conda--infer-env-from-buffer ()
   (should (= (+ 1 1) 1)))

(ert-deftest test-conda--set-python-shell-virtualenv-var ()
   (should (= (+ 1 1) 1)))

;; ;; "public" functions

(ert-deftest test-conda-env-clear-history ()
   (should (= (+ 1 1) 1)))

(ert-deftest test-conda-env-default-location ()
   (should (= (+ 1 1) 1)))

(ert-deftest test-conda-env-name-to-dir ()
  ;; conda-env-default-location
  ;; conda-env-dir-is-valid for dirs under the default location
  (cl-letf (((symbol-function 'conda-env-default-location)
             (lambda ()
               "/home/user/envs"))
            (conda-env-executables-dir "bin")
            ((symbol-function 'f-directory?)
             (lambda (dir)
               (equal dir "/home/user/envs/sample/bin"))))
   (should (equal (conda-env-name-to-dir "sample") "/home/user/envs/sample"))))

(ert-deftest test-conda-env-dir-to-name ()
   (should (= (+ 1 1) 1)))

(ert-deftest test-conda-env-candidates ()
   (should (= (+ 1 1) 1)))

(ert-deftest test-conda-env-candidates-from-dir ()
   (should (= (+ 1 1) 1)))

(ert-deftest test-conda-env-stripped-path ()
  (cl-letf (((symbol-function 'conda-env-default-location)
             (lambda ()
              "/home/user/sample")))
    (should (equal (conda-env-stripped-path
                    "/usr/bin:/usr/local/bin:/home/user/sample/bin:/home/user/anaconda/bin")
                   "/usr/bin:/usr/local/bin:/home/user/anaconda/bin"))
    (should (equal (conda-env-stripped-path
                    "/usr/bin:/usr/local/bin:/home/user/anaconda/bin")
                   "/usr/bin:/usr/local/bin:/home/user/anaconda/bin"))))

(ert-deftest test-conda-env-is-valid ()
   (should (= (+ 1 1) 1)))

(ert-deftest test-conda-env-read-name ()
  ;; TODO: test that it passes completing-read some sensible candidates, and
  ;; that the `default` that's passed through is sensible too
  (defvar test-conda-env-read-name-args nil)
  (cl-letf (((symbol-function 'completing-read)
             (lambda (prompt coll &optional pred req hist default iim)
               (setq test-conda-env-read-name-args (list prompt coll pred req hist default iim)))))
   (cl-letf (((symbol-function 'conda-env-candidates)
              (lambda ()
                '("one" "two" "three")))
             (conda-env-history (list "three" "two" "one")))
     (conda-env-read-name "prompt")
     (should (equal test-conda-env-read-name-args
                    '("prompt" ("one" "two" "three") nil t nil conda-env-history "three"))))
   (cl-letf (((symbol-function 'conda-env-candidates)
              (lambda ()
                '("one" "two" "three")))
             (conda-env-history (list)))
     (conda-env-read-name "prompt")
     (should (equal test-conda-env-read-name-args
                    '("prompt" ("one" "two" "three") nil t nil conda-env-history "one"))))
  ))

;; potentially interactive user-exposed functions

;;;;###autoload
(ert-deftest test-conda-env-deactivate ()
   (should (= (+ 1 1) 1)))

;;;;###autoload
(ert-deftest test-conda-env-activate ()
   (should (= (+ 1 1) 1)))

;;;;###autoload
(ert-deftest test-conda-env-initialize-eshell ()
   (should (= (+ 1 1) 1)))

;;;;###autoload
(ert-deftest test-conda-env-activate-for-buffer ()
   (should (= (+ 1 1) 1)))

(ert-deftest test-conda--switch-buffer-auto-activate ()
   (should (= (+ 1 1) 1)))
