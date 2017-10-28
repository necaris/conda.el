;;; conda.el --- Work with your conda environments

;; Copyright (C) 2016-2017 Rami Chowdhury
;; Author: Rami Chowdhury <rami.chowdhury@gmail.com>
;; URL: http://github.com/necaris/conda.el
;; Version: 20160914
;; Keywords: python, environment, conda
;; Package-Requires: ((emacs "24.4") (pythonic "0.1.0") (dash "2.13.0") (s "1.11.0") (f "0.18.2"))

;; Derived from James Porter's virtualenvwrapper.el (https://github.com/porterjamesj/virtualenvwrapper.el)

;;; Commentary:

;; A conda environment manager, assuming the use of Anaconda and the `conda`
;; tool.  See https://github.com/necaris/conda.el for more details.

;;; Code:
(require 'dash)
(require 's)
(require 'pythonic)
(require 'f)
(require 'eshell)

;; TODO:
;; - conda install / uninstall from emacs?
;; - want to set multiple possible home dirs for envs
;; - make this work in addition to `pew` or `virtualenvwrapper` or similar

(defgroup conda nil
  "Conda (environment) manager for Emacs."
  :group 'python)

(defcustom conda-anaconda-home
  (expand-file-name (or (getenv "ANACONDA_HOME") "~/.anaconda3/"))
  "Location of your Anaconda installation.

The default location is ~/.anaconda3/, or read from the ANACONDA_HOME
environment variable."
  :group 'conda)

(defcustom conda-system-gud-pdb-command-name
  (if (boundp 'gud-pdb-command-name)
      gud-pdb-command-name
    (setq gud-pdb-command-name "python -m pdb"))
  "Whatever `gud-pdb-command-name' is (usually \\[pdb]).")

(defcustom conda-env-home-directory conda-anaconda-home
  "Location of the directory containing the environments directory.")

(defcustom conda-env-subdirectory "envs"
  "Location of the environments subdirectory relative to ANACONDA_HOME.")

(defcustom conda-message-on-environment-switch t
  "Whether to message when switching environments. Default true.")

;; hooks -- TODO once we actually have environment creation / deletion

(defcustom conda-preactivate-hook nil
  "Hook run before a conda environment is activated.")

(defcustom conda-postactivate-hook nil
  "Hook run after a conda environment is activated.")

(defcustom conda-predeactivate-hook nil
  "Hook run before a conda environment is deactivated.")

(defcustom conda-postdeactivate-hook nil
  "Hook run after a conda environment is deactivated.")

;; internal variables that you probably shouldn't mess with

(defvar conda-env-history nil "The history of conda envs we have worked on.")

(defvar conda-env-current-name nil "Name of current conda env.")

(defvar conda-env-executables-dir  ;; copied from virtualenv.el
  (if (eq system-type 'windows-nt) "Scripts" "bin")
  "Name of the directory containing executables.  It is system dependent.")

(defvar conda-project-env-name nil  ;; placeholder for buffer-local variable
  "Current conda environment for the project.  Should always be buffer-local.")
;; ensure it's considered safe
(put 'conda-project-env-name 'safe-local-variable 'stringp)

;; internal utility functions

(defun conda--set-env-gud-pdb-command-name ()
  "When in a conda environment, call pdb as \\[python -m pdb]."
  (setq gud-pdb-command-name "python -m pdb"))

(defun conda--set-system-gud-pdb-command-name ()
  "Set the system \\[pdb] command."
  (setq gud-pdb-command-name conda-system-gud-pdb-command-name))

(defun conda--env-dir-is-valid (potential-directory)
  "Confirm that POTENTIAL-DIRECTORY is a valid conda environment."
  (let* ((xp-path-dir (file-name-as-directory potential-directory))
         (xp-bin (concat xp-path-dir conda-env-executables-dir))
         (xp-bin-exists (file-exists-p xp-bin)))
    (and potential-directory xp-bin-exists)))

(defun conda--filter-blanks (items)
  "Remove empty string items from ITEMS."
  (-filter
   (lambda (p) (not (s-blank? p)))
   items))

(defun conda--includes-path-element (env-location elem)
  "Check whether ENV-LOCATION is in the path hierarchy of ELEM."
  (not (s-contains? env-location elem)))

(defun conda--purge-history (candidates)
  "Remove history candidates that are not in CANDIDATES."
  (setq conda-env-history
        (-filter (lambda (s) (not (-contains? candidates s)))
                 conda-env-history)))

(defun conda--get-env-name ()
  "Read environment name, prompting appropriately whether an env is active now."
  (let* ((current conda-env-current-name)
         (prompt (if current
                     (format "Choose a conda environment (currently %s): " current)
                   "Choose a conda environment: ")))
    (conda-env-read-name prompt)))

(defun conda--check-executable ()
  "Verify there is a conda executable available, throwing an error if not."
  (unless (executable-find "conda")
    (error "There doesn't appear to be a conda executable on your exec path.  A
    common cause of problems like this is GUI Emacs not having environment
    variables set up like the shell.  Check out
    https://github.com/purcell/exec-path-from-shell for a robust solution to
    this problem")))

(defun conda--find-env-yml (dir)
  "Find an environment.yml in DIR or its parent directories."
  ;; TODO: implement an optimized finder with e.g. projectile?
  (let* ((contains-env-yml-p (lambda (p)
                               (f-exists? (f-expand "environment.yml" p))))
         (containing-path (f-traverse-upwards contains-env-yml-p dir)))
    (if containing-path
        (f-expand "environment.yml" containing-path)
      nil)))

(defun conda--get-name-from-env-yml (filename)
  "Pull the `name` property out of the YAML file at FILENAME."
  (when filename
    (let ((env-yml-contents (f-read-text filename)))
      (if (string-match "name:[ ]*\\(\\w+\\)[ ]*$" env-yml-contents)
          (match-string 1 env-yml-contents)
        nil))))

(defun conda--infer-env-from-buffer ()
  "Search up the project tree for an `environment.yml` defining a conda env."
  (let ((current-dir (f-dirname (buffer-file-name))))
    (conda--get-name-from-env-yml (conda--find-env-yml current-dir))))

(defun conda--set-python-shell-virtualenv-var (location)
  "Set appropriate Python shell variable to LOCATION."
  ;; Emacs 25.1+ defines python-shell-virtualenv-root
  (if (boundp 'python-shell-virtualenv-root)
      (setq python-shell-virtualenv-root location)
    (setq python-shell-virtualenv-path location)))

(defun conda-env-clear-history ()
  "Clear the history of conda environments that have been activated."
  (setq conda-env-history nil))

(defun conda-env-location ()
  "Location of the conda environments."
  (concat (file-name-as-directory conda-anaconda-home) conda-env-subdirectory))

(defun conda-env-name-to-dir (name)
  "Translate NAME into the directory where the environment is located."
  (let* ((env-possibilities (list (conda-env-location))) ;; can add venv-location?
         (potential-dirs (mapcar (lambda (x) (concat x "/" name))
                                 env-possibilities))
         (valid-dirs (-filter 'conda--env-dir-is-valid potential-dirs)))
    (if (> (length valid-dirs) 0)
        (expand-file-name (car valid-dirs))
      (error "No such conda environment: %s" name))))

(defun conda-env-dir-to-name (dir)
  "Extract the name of a conda environment from DIR."
  (let* ((pieces (split-string dir "/"))
        (non-blank (conda--filter-blanks pieces)))
    (car (last non-blank))))

(defun conda-env-candidates ()
  "Fetch all the candidate environments."
  (let ((candidates (conda-env-candidates-from-dir (conda-env-location))))
    (when (not (eq (length (-distinct candidates))
                   (length candidates)))
      (error "Some envs have the same name!"))
    candidates))

(defun conda-env-candidates-from-dir (dir)
  "Return a list of candidate environment names from DIR."
  (let ((proper-dir (file-name-as-directory (expand-file-name dir))))
    (if (not (file-accessible-directory-p proper-dir))
	(list) ;; an empty list of candidates
      (-filter (lambda (s)
		 (let ((subdir (concat proper-dir s)))
		   (car (file-attributes
			 (concat (file-name-as-directory subdir)
				 conda-env-executables-dir)))))
	       (directory-files proper-dir nil "^[^.]")))))

(defun conda-env-stripped-path (path)
  "Strip PATH of anything inserted by the current environment."
  (let* ((xp-location (expand-file-name (conda-env-location)))
         (proper-location (file-name-as-directory xp-location)))
    (-filter (lambda (p)
	       (and (not (null p))
		    (conda--includes-path-element proper-location p)))
             path)))

(defun conda-env-is-valid (name)
  "Check whether NAME points to a valid conda environment."
  (conda--env-dir-is-valid (conda-env-name-to-dir name)))

(defun conda-env-read-name (prompt)
  "Do a completing read to get a candidate name, prompting with PROMPT."
  (let ((candidates (conda-env-candidates)))
    ;; purge history of no longer existant candidates first
    (conda--purge-history candidates)
    (completing-read prompt
                     candidates nil t nil
                     'conda-env-history
                     (or (car conda-env-history)
                         (car candidates)))))

;; potentially interactive user-exposed functions

;;;###autoload
(defun conda-env-deactivate ()
  "Deactivate the current conda env."
  (interactive)
  (run-hooks 'conda-predeactivate-hook)
  (conda--set-python-shell-virtualenv-var nil)
  (setq exec-path (conda-env-stripped-path exec-path))
  (setenv "PATH" (s-join path-separator
                  (conda-env-stripped-path
                   (s-split path-separator (getenv "PATH")))))
  (setenv "VIRTUAL_ENV" nil)
  (setq conda-env-current-name nil)
  (setq eshell-path-env (getenv "PATH"))
  (conda--set-system-gud-pdb-command-name)
  (run-hooks 'conda-postdeactivate-hook)
  (when (called-interactively-p 'interactive)
    (message "conda env deactivated")))

;;;###autoload
(defun conda-env-activate (&optional name)
  "Switch to environment NAME, prompting if called interactively."
  (interactive)
  (let ((env-name (or name (conda--get-env-name))))
    (if (not (conda-env-is-valid env-name))
        (error "Invalid conda environment specified: %s" env-name)
      ;; first, deactivate any existing env
      (conda-env-deactivate)
      ;; set the state of the environment, including setting (or re-setting)
      ;; a buffer-local variable that allows us to skip discovery when we
      ;; switch back into the buffer.
      (setq conda-env-current-name env-name)
      (set (make-local-variable 'conda-project-env-name) env-name)
      ;; run hooks
      (run-hooks 'conda-preactivate-hook)
      ;; push it onto the history
      (add-to-list 'conda-env-history conda-env-current-name)
      (let* ((env-dir (conda-env-name-to-dir env-name))
             (env-exec-dir (concat (file-name-as-directory env-dir)
                                   conda-env-executables-dir)))
        ;; Use pythonic to activate the environment so that anaconda-mode and
        ;; others know how to work on this
        (pythonic-activate env-dir)
        ;; setup the python shell
        (conda--set-python-shell-virtualenv-var env-dir)
        ;; setup emacs exec-path
        (add-to-list 'exec-path env-exec-dir)
        ;; setup the environment for subprocesses, eshell, etc
        (setenv "PATH" (concat env-exec-dir path-separator (getenv "PATH")))
        (setq eshell-path-env (getenv "PATH"))
        (setenv "VIRTUAL_ENV" env-dir)
        (conda--set-env-gud-pdb-command-name)
        (run-hooks 'conda-postactivate-hook)))
    (if (or conda-message-on-environment-switch (called-interactively-p 'interactive))
	(message "Switched to conda environment: %s" env-name))))


;; for hilarious reasons to do with bytecompiling, this has to be here
;; instead of below
(defmacro conda-with-env (name &rest forms)
  "With conda env NAME active, evaluate FORMS."
  `(progn
     (let ((prev-dir default-directory)
           (prev-env conda-env-current-name))
       (conda-env-activate ,name) ;; switch it up
       (unwind-protect
           (progn
             ,@forms) ;; evaluate forms
         (if prev-env ;; switch back
             (conda-env-activate prev-env)
           (conda-env-deactivate))))))


;;;###autoload
(defun conda-env-list ()
  "List all available conda environments in a temp buffer."
  (interactive)
  (with-output-to-temp-buffer
      "*Conda envs*"
      (princ (s-join "\n" (conda-env-candidates)))))


;;;###autoload
(defun conda-with-env-shell-command (name command) ;; FIXME
  "With environment NAME active, execute the shell string COMMAND."
  (conda-with-env name (shell-command command)))


;; Code for setting up interactive shell and eshell

;; interactive shell

;;;###autoload
(defun conda-env-shell-init (process)
  "Activate the current env in a newly opened shell PROCESS."
  (comint-send-string
   process
   (concat "source activate " conda-env-current-name)))

(defun conda--shell-strip-env (orig-fun &rest args)
  "Use the environment without env to start a new shell, passing ORIG-FUN ARGS."
  (let* ((buffer-name (or buffer "*shell*"))
           (buffer-exists-already (get-buffer buffer-name)))
      (if (or buffer-exists-already (not conda-env-current-name))
          (apply orig-fun args)
        (progn (setenv "PATH"
                       (s-join
                        path-separator
                        (conda-env-stripped-path (s-split path-separator (getenv "PATH")))))
               (setenv "VIRTUAL_ENV" nil)
               ad-do-it
               (conda-env-shell-init buffer-name)
               (setenv "PATH"
                       (concat
                        (file-name-as-directory
                         (conda-env-name-to-dir conda-env-current-name))
                        conda-env-executables-dir
                        path-separator
                        (getenv "PATH")))
               (setenv "VIRTUAL_ENV" (conda-env-name-to-dir conda-env-current-name))))))

;;;###autoload
(defun conda-env-initialize-interactive-shells ()
  "Configure interactive shells for use with conda.el."
  (advice-add 'shell :around #'conda--shell-strip-env))

;; eshell

(eval-and-compile
  (defun conda--gen-fun (command)
    `(defun ,(intern (format "pcomplete/eshell-mode/%s" command)) ()
       (pcomplete-here* (conda-env-candidates)))))

(defmacro conda--make-pcompletions (commands)
  "Make eshell pcompletions for COMMANDS."
  `(progn ,@(-map #'conda--gen-fun commands)))

;;;###autoload
(defun conda-env-initialize-eshell ()
  "Configure eshell for use with conda.el."
  ;; make emacs and eshell share an environment
  (setq eshell-modify-global-environment t)
  ;; set eshell path
  (setq eshell-path-env (getenv "PATH"))
  ;; alias functions
  (defun eshell/activate (arg) (conda-env-activate arg))
  (defun eshell/deactivate () (conda-env-deactivate))
  ;; (defun eshell/rmvirtualenv (&rest args) (apply #'conda-env-rmvirtualenv args))
  ;; (defun eshell/mkvirtualenv (&rest args) (apply #'conda-env-mkvirtualenv args))
  (defun eshell/lsvirtualenv () (conda-env-list))
  ;; make completions work
  (conda--make-pcompletions ("activate"))
  (message "Eshell virtualenv support initialized."))

;;;###autoload
(defun conda-env-activate-for-buffer ()
  "Activate the conda environment implied by the current buffer.

This can be set by a buffer-local or project-local variable (e.g. a
`.dir-locals.el` that defines `conda-project-env-name`), or inferred from an
`environment.yml` or similar at the project level."
  (interactive)
  (let ((env-name (if conda-project-env-name
                      conda-project-env-name
                    (conda--infer-env-from-buffer))))
    (if (not env-name)
      (if conda-message-on-environment-switch
        (message "No conda environment for <%s>" (buffer-file-name)))
      (conda-env-activate env-name))))

(defun conda--switch-buffer-auto-activate (&rest args)
  "Add conda env activation if a buffer has a file, handling ARGS."
  (let ((filename (buffer-file-name)))
  (when filename
    ; (message "switch-buffer auto-activating on <%s>" filename)
    (with-demoted-errors "Error: %S"
      (conda-env-activate-for-buffer)))))

;;;###autoload
(define-minor-mode conda-env-autoactivate-mode
  "Toggle conda-env-autoactivate mode.

This mode automatically tries to activate a conda environment for the current
buffer."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  nil
  ;; The minor mode bindings.
  nil
  ;; Kwargs
  :group 'conda
  :global t
  ;; Forms
  (if conda-env-autoactivate-mode ;; already on, now switching off
      (advice-add 'switch-to-buffer :after #'conda--switch-buffer-auto-activate)
    (advice-remove 'switch-to-buffer #'conda--switch-buffer-auto-activate)))

(provide 'conda)
;;; conda.el ends here
