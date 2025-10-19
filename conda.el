;; -*- lexical-binding: t; -*-
;;; conda.el --- Work with your conda environments

;; Copyright (C) 2016-2024 Rami Chowdhury
;; Author: Rami Chowdhury <rami.chowdhury@gmail.com>
;; URL: http://github.com/necaris/conda.el
;; Version: 0.5
;; Package-Version: 0.5
;; Keywords: languages, local, tools, python, environment, conda
;; Package-Requires: ((emacs "25.1") (pythonic "0.1.0") (dash "2.13.0") (s "1.11.0") (f "0.18.2"))

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
(require 'em-dirs)
(require 'json)

;; TODO:
;; - conda install / uninstall from emacs?

(defgroup conda nil
  "Conda (environment) manager for Emacs."
  :group 'python)

(defcustom conda-home-candidates
  (list "~/.anaconda"
        "~/.anaconda3"
        "~/.miniconda"
        "~/.miniconda3"
        "~/.miniforge3"
        "~/.mambaforge"
        "~/anaconda3"
        "~/miniconda3"
        "~/miniforge3"
        "~/mambaforge"
        "~/opt/miniconda3"
        "/opt/miniconda3"
        "/usr/bin/anaconda3"
        "/usr/local/anaconda3"
        "/usr/local/miniconda3"
        "/usr/local/Caskroom/miniconda/base"
        "~/.conda")
  "Location of possible candidates for conda environment directory."
  :type '(list string)
  :group 'conda)

;; TODO: find some way to replace this with `(alist-get 'root_prefix (conda--get-config))`
;; unfortunately right now (conda--get-executable-path) relies on this!
(defcustom conda-anaconda-home
  (or (cl-loop for dir in (cons (getenv "ANACONDA_HOME") conda-home-candidates)
               if (and dir (file-directory-p dir))
               return (setq conda-anaconda-home (expand-file-name dir)
                            conda-env-home-directory (expand-file-name dir)))
      (message "Cannot find Anaconda installation"))
  "Location of your conda installation.

Iterate over default locations in CONDA-HOME-CANDIDATES, or read from the
ANACONDA_HOME environment variable."
  :type 'directory
  :group 'conda)

(defcustom conda-system-gud-pdb-command-name
  (if (boundp 'gud-pdb-command-name)
      gud-pdb-command-name
    (setq gud-pdb-command-name "python -m pdb"))
  "Whatever `gud-pdb-command-name' is (usually \\[pdb])."
  :type 'string
  :group 'conda)

(defcustom conda-message-on-environment-switch t
  "Whether to message when switching environments.  Default true."
  :type 'boolean
  :group 'conda)

(defcustom conda-activate-base-by-default nil
  "Whether to activate the base environment by default if no other is preferred.
Default nil."
  :type 'boolean
  :group 'conda)

;; hooks -- TODO once we actually have environment creation / deletion

(defcustom conda-preactivate-hook nil
  "Hook run before a conda environment is activated."
  :type 'hook
  :group 'conda)

(defcustom conda-postactivate-hook nil
  "Hook run after a conda environment is activated."
  :type 'hook
  :group 'conda)

(defcustom conda-predeactivate-hook nil
  "Hook run before a conda environment is deactivated."
  :type 'hook
  :group 'conda)

(defcustom conda-postdeactivate-hook nil
  "Hook run after a conda environment is deactivated."
  :type 'hook
  :group 'conda)

;; internal variables that you probably shouldn't mess with

(defvar conda-env-history nil "The history of conda envs we have worked on.")

(defvar conda-env-current-name nil "Name of current conda env.")

(defvar conda-env-current-path nil "Path of current conda env.")

(defvar conda-env-executables-dir  ;; copied from virtualenv.el
  (if (eq system-type 'windows-nt) "Scripts" "bin")
  "Name of the directory containing executables.  It is system dependent.")

(defvar conda-env-meta-dir "conda-meta"
  "Name of the directory containing metadata.
This should be consistent across platforms.")

(defvar conda-env-name-for-buffer nil  ;; placeholder for buffer-local variable
  "Current conda environment for the project.  Should always be buffer-local.")
;; ensure it's considered safe
(put 'conda-env-name-for-buffer 'safe-local-variable 'stringp)

(defvar conda-mode-line '(:propertize
                          (:eval (when conda-env-current-name (concat "$" conda-env-current-name " ")))
                          help-echo "Current conda env"))

;; internal utility functions

(defvar conda--executable-path nil
  "Cached copy of full path to Conda binary.
Set for the lifetime of the process.")

(defun conda--get-executable-path ()
  "Return full path to Conda binary, or throw an error if it can't be found.
Cached for the lifetime of the process."
  (if (not (eq conda--executable-path nil))
      conda--executable-path
    (setq conda--executable-path
          (cond
           ((locate-file "conda" (list (f-join conda-anaconda-home conda-env-executables-dir)) exec-suffixes 'executable))
	   ((locate-file "mamba" (list (f-join conda-anaconda-home conda-env-executables-dir)) exec-suffixes 'executable))
           ((executable-find "conda"))
           ((executable-find "mamba"))
           (t (error
               "There doesn't appear to be a conda or mamba executable on your exec path.  A common
 cause of problems like this is GUI Emacs not having environment variables set up like the
 shell.  Check out https://github.com/purcell/exec-path-from-shell for a robust solution to
 this problem"))))))

(defvar conda--installed-version nil
  "Cached copy of installed Conda version.
Set for the lifetime of the process.")

(defun conda--get-installed-version()
  "Return currently installed Conda version.
Cached for the lifetime of the process."
  (if (not (eq conda--installed-version nil))
      conda--installed-version
    (let ((version-output (with-temp-buffer
                            (call-process (conda--get-executable-path) nil '(t nil) nil "-V")
                            (buffer-string))))
      (condition-case err
          (s-with version-output
            (s-trim)
            (s-split " ")
            (cadr)
            (setq conda--installed-version))
        (error "Could not parse Conda version: %s (output was %s)" err version-output)))))

(defun conda--supports-json-activator ()
  "Does the installed Conda version support JSON activation?
See https://github.com/conda/conda/blob/master/CHANGELOG.md#484-2020-08-06."
  (version< "4.8.3" (conda--get-installed-version)))

(defun conda--supports-old-activate-format ()
  "Does the installed Conda support the deprecated `..activate' command format?"
  (version<= (conda--get-installed-version) "4.12.0"))

(defun conda--call-json (&rest args)
  "Call Conda with ARGS, assuming we return JSON."
  (let* ((conda (conda--get-executable-path))
         (output (with-temp-buffer
                   ;; We set the `destination' to ignore stderr -- this may come
                   ;; back to bite us if anything important is communicated
                   ;; there
                   (apply #'call-process
                          (append (list conda nil '(t nil) nil) args))
                   (buffer-string))))
    (condition-case err
        (if (and (require 'json) (fboundp 'json-parse-string))
            (json-parse-string output :object-type 'alist :null-object nil)
          (json-read-from-string output))
      (error "Could not parse %s as JSON: %s" output err))))

(defvar conda--config nil
  "Cached copy of configuration that Conda sees (including `condarc', etc).
Set for the lifetime of the process.")

(defun conda--get-config (&optional force-reload)
  "Return current configuration. Cached for the process' lifetime, unless FORCE-RELOAD."
  (if (and (not force-reload) (not (eq conda--config nil)))
      conda--config
    (let ((cfg (conda--call-json "config" "--show" "--json")))
      (setq conda--config cfg))))

;; (conda--get-config)
;; keys envs-dirs and root-prefix seem immediately relevant

(defun conda--update-env-from-params (params)
  "Update the environment from PARAMS."
  (let ((exports (or (conda-env-params-vars-export params) '())))
    (mapc (lambda (pair)
            (let ((inhibit-message t))
	      (message "About to set %s to %s" (car pair) (cdr pair)))
            (setenv (format "%s" (car pair)) (format "%s" (cdr pair))))
          exports))
  (let ((unsets (or (conda-env-params-vars-unset params) '())))
    (mapc (lambda (arg)
            (let ((inhibit-message t))
	      (message "About to unset %s" arg))
            (setenv (format "%s" arg) nil))
          unsets))
  (let ((sets (or (conda-env-params-vars-set params) '())))
    (mapc (lambda (pair)
            (let ((inhibit-message t))
	      (message "About to set %s to %s" (car pair) (cdr pair)))
            (setenv (format "%s" (car pair)) (format "%s" (cdr pair))))
          sets)))

(defun conda--set-env-gud-pdb-command-name ()
  "When in a conda environment, call pdb as \\[python -m pdb]."
  (setq-default gud-pdb-command-name "python -m pdb"))

(defun conda--set-system-gud-pdb-command-name ()
  "Set the system \\[pdb] command."
  (setq-default gud-pdb-command-name conda-system-gud-pdb-command-name))

(defun conda--env-dir-is-valid (candidate)
  "Confirm that CANDIDATE is a valid conda environment."
  (let ((dir (file-name-as-directory candidate)))
    (and (not (s-blank? candidate))
         (f-directory? dir)
         (or (f-directory? (concat dir conda-env-executables-dir))
             (f-directory? (concat dir conda-env-meta-dir))))))

(defun conda--filter-blanks (items)
  "Remove empty strings from ITEMS."
  (-filter (lambda (p)
             (not (s-blank? p)))
           items))

(defun conda--purge-history (candidates)
  "Remove history candidates that are not in CANDIDATES."
  (setq conda-env-history
        (-filter (lambda (s)
                   (-contains? candidates s))
                 conda-env-history)) )

(defun conda--read-env-name ()
  "Read environment name, prompting appropriately whether an env is active now."
  ;; TODO FEATURE: does this need to be inferred from the directory?
  (conda-env-read-name
   (format "Choose a conda environment%s: "
           (if conda-env-current-name
               (format " (currently %s)" conda-env-current-name)
             ""))))

(defcustom conda-env-yaml-base-name "environment"
  "Base filename for acceptable Conda environment YAML files."
  :type 'string
  :safe #'stringp
  :group 'conda)

(make-variable-buffer-local
 'conda-env-yaml-base-name)

(defun conda--env-yaml-expand (dir)
  "If DIR contains an environment YAML file named as `conda-env-yaml-base-name',
its absolute path is returned. Otherwise, it returns nil."
  (seq-find #'f-exists?
   (list (f-expand (concat conda-env-yaml-base-name ".yaml") dir)
         (f-expand (concat conda-env-yaml-base-name ".yml") dir))))

(defun conda--find-env-yaml (&optional dir)
  "Finds the path of an environment YAML file in DIR,
or one of its parent directories, or else returns nil."
  ;; TODO: implement an optimized finder with e.g. projectile? Or a series of
  ;; finder functions, that stop at the project root when traversing
  (let* ((dir (or dir default-directory))
         (containing-path (f-traverse-upwards #'conda--env-yaml-expand dir)))
    (if containing-path (conda--env-yaml-expand containing-path))))

(defun conda--get-name-from-env-yaml (&optional file-path)
  "If FILE-PATH is non-nil, retrieves the `name` property
from the YAML file FILE-PATH. Otherwise, returns nil."
  ;; TODO: find a better way than slurping it in and using a regex...
  (when (and file-path (file-exists-p file-path))
    (let ((env-yaml-contents (f-read-text file-path)))
      (if (string-match "name:[ ]*\\([A-z0-9-_.]+\\)[ ]*$" env-yaml-contents)
          (match-string 1 env-yaml-contents)
        nil))))

(defun conda--infer-env-from-buffer ()
  "Infer conda environment name by looking for an environment
YAML file in parent directories, or in `default-directory`.

Otherwise use \"base\" environment if available."
  (let* ((filename (buffer-file-name)))
    (or (conda--get-name-from-env-yaml
         (conda--find-env-yaml
          (and filename (f-dirname filename))))
        (if (or conda-activate-base-by-default
                (alist-get 'auto_activate_base
                           (conda--get-config)))
            "base"))))

(cl-defstruct conda-env-params
  "Parameters necessary for (de)activating a Conda environment."
  path
  vars-export
  vars-set
  vars-unset
  scripts-activate
  scripts-deactivate)

(defun conda--call-json-subcommand (subcommand &rest subcommand-args)
  "Call Conda SUBCOMMAND with SUBCOMMAND-ARGS returning JSON.
The most common additional argument is the environment directory."
  (let* ((fmt (format "shell.%s+json"  (if (eq system-type 'windows-nt) "cmd.exe" "posix")))
         (args (append (list fmt subcommand) subcommand-args)))
    (apply #'conda--call-json args)))

(defun conda--get-activation-parameters (env-dir)
  "Return activation values for the environment in ENV-DIR.
Returns a `conda-env-params' struct.  At minimum, this will contain an
updated PATH."
  (if (conda--supports-json-activator)
      (let ((result (conda--call-json-subcommand "activate" env-dir)))
        (make-conda-env-params
         :path (s-join path-separator (alist-get 'PATH (alist-get 'path result)))
         :vars-export (alist-get 'export (alist-get 'vars result))
         :vars-set (alist-get 'set (alist-get 'vars result))
         :vars-unset (alist-get 'unset (alist-get 'vars result))
         :scripts-activate (alist-get 'activate (alist-get 'scripts result))
         :scripts-deactivate  (alist-get 'deactivate (alist-get 'scripts result))))
    (if (not (conda--supports-old-activate-format))
        (error "Installed Conda version supports neither JSON nor the old format.  This shouldn't happen!")
      (make-conda-env-params
       :path (concat
              (conda--get-deprecated-path-prefix env-dir)
              path-separator
              (getenv "PATH"))))))

(defun conda--get-deactivation-parameters (env-dir)
  "Return activation values for the environment in ENV-DIR.
Returns a `conda-env-params' struct.  At minimum, this will contain an
updated PATH."
  (if (conda--supports-json-activator)
      (let ((result (conda--call-json-subcommand "deactivate")))
        (make-conda-env-params
         :path (s-join path-separator (alist-get 'PATH (alist-get 'path result)))
         :vars-export (alist-get 'export (alist-get 'vars result))
         :vars-set (alist-get 'set (alist-get 'vars result))
         :vars-unset (alist-get 'unset (alist-get 'vars result))
         :scripts-activate (alist-get 'activate (alist-get 'scripts result))
         :scripts-deactivate  (alist-get 'deactivate (alist-get 'scripts result))))
    (make-conda-env-params
     :path (s-with (getenv "PATH")
             (s-split path-separator)
             (conda-env-stripped-path)
             (s-join path-separator)))))

(defun conda--get-deprecated-path-prefix (env-dir)
  "Get a path string to utilize the conda env in ENV-DIR.
Use the platform's native path separator.  Don't use this -- prefer
`conda--get-activation-parameters' to this where possible."
  (s-trim
   (with-output-to-string
     (let ((conda-anaconda-home-tmp conda-anaconda-home))
       (with-current-buffer standard-output
         (let* ((conda (conda--get-executable-path))
                (cmd "%s ..activate \"%s\" \"%s\"")
                (sh (if (eq system-type 'windows-nt) "cmd.exe" "bash"))
                (command (format cmd conda sh env-dir))
                (return-code (process-file shell-file-name nil '(t nil) nil shell-command-switch command)))
           (unless (= 0 return-code)
             (error (format "Error: executing command \"%s\" produced error code %d" command return-code)))))))))

(defun conda--eshell-update-path ()
  "Update `eshell-path-env' from the current `PATH'."
  (if (version<= emacs-version "29.1")
      (setq eshell-path-env (getenv "PATH"))
    (setq-default eshell-get-path (getenv "PATH"))))

;; "public" functions

(defun conda-env-clear-history ()
  "Clear the history of conda environments that have been activated."
  (setq conda-env-history nil))

(defun conda-env-default-location ()
  "Default location of the conda environments -- under the Anaconda installation."
  (let ((candidates (alist-get 'envs_dirs (conda--get-config))))
    (f-full (aref candidates 0))))

(defun conda-env-name-to-dir (name)
  "Translate NAME to the directory where the environment is located."
  (if (and (string= name "base")
           (conda--env-dir-is-valid conda-anaconda-home))
      (file-name-as-directory (expand-file-name conda-anaconda-home))
    (let* ((default-location (file-name-as-directory (conda-env-default-location)))
           (initial-possibilities (list name (concat default-location name)))
           (possibilities (if (boundp 'venv-location)
                              (if (stringp venv-location)
                                  (cons venv-location initial-possibilities)
                                (nconc venv-location initial-possibilities))
                            initial-possibilities))
           (matches (-filter 'conda--env-dir-is-valid possibilities)))
      (if (> (length matches) 0)
          (file-name-as-directory (expand-file-name (car matches)))
        (error "No such conda environment: %s" name)))))

(defun conda-env-dir-to-name (dir)
  "Extract the name of a conda environment from DIR."
  ;; TODO FEATURE: only do this extraction if it's under the default envs dir
  (cond ((file-equal-p dir conda-anaconda-home) "base")
        ((f-ancestor-of? (conda-env-default-location) dir)
         (let* ((pieces (f-split dir))
                (non-blank (conda--filter-blanks pieces)))
           (car (last non-blank))))
        (t dir)))

(defun conda-env-candidates ()
  "Fetch all the candidate environments."
  ;; TODO FEATURE: include the current one if it's valid
  (let ((candidates (conda-env-candidates-from-dir (conda-env-default-location))))
    ;; Add 'base' env to candidates list, which corresponds to
    ;; `conda-anaconda-home' path.
    (when (conda--env-dir-is-valid conda-anaconda-home)
      (push "base" candidates))
    (when (not (eq (length (-distinct candidates))
                   (length candidates)))
      (error "Some envs have the same name!"))
    candidates))

(defun conda-env-candidates-from-dir (dir)
  "Return a list of candidate environment names from DIR."
  (let ((envs-dir (file-name-as-directory (expand-file-name dir))))
    (if (not (file-accessible-directory-p envs-dir))
        (list) ;; an empty list of candidates
      (-filter (lambda (c)
                 (conda--env-dir-is-valid (concat envs-dir c)))
               (directory-files envs-dir nil "^[^.]")))))

(defun conda-env-stripped-path (path-or-path-elements)
  "Strip PATH-OR-PATH-ELEMENTS of anything inserted by the current environment.
Returns a list of new path elements."
  (let ((current-env-entry (concat (file-name-as-directory
				    (expand-file-name conda-env-current-path))
				   conda-env-executables-dir))
        (path-elements (if (listp path-or-path-elements)
                           path-or-path-elements
                         (s-split path-separator path-or-path-elements))))
    (-filter (lambda (e)
               (not (s-equals? current-env-entry (directory-file-name e))))
             path-elements)))

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

(defun conda-mode-line-setup ()
  "Setup a basic mode-line display of current env."
  (add-to-list 'mode-line-misc-info  conda-mode-line))

;; potentially interactive user-exposed functions

(defun conda-reload-config ()
  "Force-reloads the Conda configuration and displays it."
  (interactive)
  (let ((cfg (conda--get-config t)))
    (message cfg)))

;;;###autoload
(defun conda-env-deactivate ()
  "Deactivate the current conda env."
  (interactive)
  (if (not (bound-and-true-p conda-env-current-path))
      (message "No Conda environment is active")
    (run-hooks 'conda-predeactivate-hook)
    (setq python-shell-virtualenv-root nil)
    (let ((params (conda--get-deactivation-parameters conda-env-current-path))
          (env-name conda-env-current-name))
      (if (not (eq nil (conda-env-params-vars-export params)))
          (conda--update-env-from-params params)
        (progn ;; otherwise we fall back to legacy heuristics
          (setenv "VIRTUAL_ENV" nil)
          (setenv "CONDA_PREFIX" nil)))
      (setq exec-path (s-split (if (eq system-type 'windows-nt) ";" ":" )
                               (conda-env-params-path params)))
      (setenv "PATH" (conda-env-params-path params))
    (setq conda-env-current-path nil)
    (setq conda-env-current-name nil)
    (conda--eshell-update-path)
    (conda--set-system-gud-pdb-command-name)
    (run-hooks 'conda-postdeactivate-hook)
    (when (called-interactively-p 'interactive)
      (message "Deactivated Conda environment <%s>" env-name)))))

;;;###autoload
(defun conda-env-activate (&optional name)
  "Switch to environment NAME, prompting if called interactively."
  (interactive)
  (let* ((env-name (or name (conda--read-env-name)))
         (env-dir (conda-env-name-to-dir env-name)))
    (conda-env-activate-path env-dir)))

;;;###autoload
(defun conda-env-activate-path (&optional path)
  "Switch to environment PATH, prompting if called interactively."
  (interactive)
  (let ((env-path (or path (read-directory-name "Conda environment directory: "))))
    (if (not (conda--env-dir-is-valid env-path))
        (error "Invalid conda environment path specified: %s" env-path)
      ;; first, deactivate any existing env
      (conda-env-deactivate)
      ;; set the state of the environment, including setting (or re-setting)
      ;; a buffer-local variable that allows us to skip discovery when we
      ;; switch back into the buffer.
      (setq conda-env-current-path env-path)
      (setq conda-env-current-name (conda-env-dir-to-name env-path))
      (set (make-local-variable 'conda-project-env-path) env-path)
      ;; run hooks
      (run-hooks 'conda-preactivate-hook)
      ;; push it onto the history
      (add-to-list 'conda-env-history conda-env-current-name)
      (let* ((env-dir (expand-file-name env-path))
             (env-exec-dir (concat (file-name-as-directory env-dir)
                                   conda-env-executables-dir)))
        ;; Use pythonic to activate the environment so that anaconda-mode and
        ;; others know how to work on this
        (pythonic-activate env-dir)
        (setq python-shell-virtualenv-root env-dir)
        (let ((params (conda--get-activation-parameters env-dir))
	      (inhibit-message t))
          (if (not (eq nil (conda-env-params-vars-export params)))
              (conda--update-env-from-params params)
            (progn ;; otherwise we fall back to legacy heuristics
              (setenv "VIRTUAL_ENV" env-dir)
              (setenv "CONDA_PREFIX" env-dir)))
          (setq exec-path (s-split (if (eq system-type 'windows-nt) ";" ":")
                                   (conda-env-params-path params)))
          (setenv "PATH" (conda-env-params-path params)))
        (conda--eshell-update-path)
        (conda--set-env-gud-pdb-command-name)
        (run-hooks 'conda-postactivate-hook)))
    (if (or conda-message-on-environment-switch (called-interactively-p 'interactive))
        (message "Switched to conda environment: %s" env-path))))

;; for hilarious reasons to do with bytecompiling, this has to be here
;; instead of below
(defmacro conda-with-env (name &rest forms)
  "With conda env NAME active, evaluate FORMS."
  `(progn
     (let ((prev-env-path conda-env-current-path))
       (conda-env-activate ,name) ;; switch it up
       (unwind-protect
           (progn
             ,@forms)
         (if prev-env-path
             (conda-env-activate-path prev-env-path)
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
  ;; TODO: make sure the shell has been set up for `conda activate`!
  ;; Do we need to `eval' the conda activation script every time?
  (let* ((activate-command (if (eq system-type 'windows-nt)
                               '("activate")
                             '("conda" "activate")))
         (full-command (append activate-command `(,conda-env-current-name "\n")))
         (command-string (combine-and-quote-strings full-command)))
    (comint-send-string process command-string)))

;;;###autoload
(defun conda-env-eshell-prompt ()
  "An Eshell prompt function to insert the active Conda environment."
  (concat
   (when (and (boundp 'conda-env-current-name)
              conda-env-current-name)
     (concat "(" conda-env-current-name ") "))
   (abbreviate-file-name (eshell/pwd))
   (if (= (user-uid) 0)
       " # "
     " $ ")))

(defun conda--shell-strip-env (orig-fun &rest args)
  "Use the environment without env to start a new shell, passing ORIG-FUN ARGS."
  (let* ((buffer (car args))
         (buffer-name (or buffer "*shell*"))
         (buffer-exists-already (get-buffer buffer-name)))
    (if (or buffer-exists-already (not conda-env-current-path))
        (apply orig-fun args)
      (progn (setenv "PATH"
                     (s-join
                      path-separator
                      (conda-env-stripped-path (s-split path-separator (getenv "PATH")))))
             (setenv "VIRTUAL_ENV" nil)
             (apply orig-fun args)
             (conda-env-shell-init buffer-name)
             (setenv "PATH"
                     (concat
                      (file-name-as-directory conda-env-current-path)
                      conda-env-executables-dir
                      path-separator
                      (getenv "PATH")))
             (setenv "VIRTUAL_ENV" conda-env-current-path)))))

;;;###autoload
(defun conda-env-initialize-interactive-shells ()
  "Configure interactive shells for use with conda.el."
  (advice-add 'shell :around #'conda--shell-strip-env))

;; vterm

;;;###autoload
(defun conda-env-vterm-init (buffer-name)
  "Activate the current env in a newly opened vterm BUFFER-NAME."
  (when (and conda-env-current-name
             (fboundp 'vterm-send-string))
    (let* ((activate-command (if (eq system-type 'windows-nt)
                                 "activate"
                               "conda activate"))
           (command-string (format "%s %s\n" activate-command conda-env-current-name)))
      (with-current-buffer buffer-name
        (vterm-send-string command-string)))))

(defun conda--vterm-strip-env (orig-fun &rest args)
  "Use the environment without env to start a new vterm, passing ORIG-FUN ARGS."
  (let* ((buffer (car args))
         (buffer-name (or buffer (generate-new-buffer-name "*vterm*")))
         (buffer-exists-already (get-buffer buffer-name)))
    (if (or buffer-exists-already (not conda-env-current-path))
        (apply orig-fun args)
      (progn (setenv "PATH"
                     (s-join
                      path-separator
                      (conda-env-stripped-path (s-split path-separator (getenv "PATH")))))
             (setenv "VIRTUAL_ENV" nil)
             (apply orig-fun args)
             ;; Wait a bit for vterm to initialize before sending the activation command
             (run-with-timer 0.1 nil
                             (lambda ()
                               (conda-env-vterm-init buffer-name)))
             (setenv "PATH"
                     (concat
                      (file-name-as-directory conda-env-current-path)
                      conda-env-executables-dir
                      path-separator
                      (getenv "PATH")))
             (setenv "VIRTUAL_ENV" conda-env-current-path)))))

;;;###autoload
(defun conda-env-initialize-vterm ()
  "Configure vterm for use with conda.el."
  (advice-add 'vterm :around #'conda--vterm-strip-env))

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
  (setq-default eshell-modify-global-environment t)
  ;; set eshell path
  (conda--eshell-update-path)
  ;; alias functions
  (defun eshell/activate (arg) (conda-env-activate arg))
  (defun eshell/deactivate () (conda-env-deactivate))
  ;; (defun eshell/rmvirtualenv (&rest args) (apply #'conda-env-rmvirtualenv args))
  ;; (defun eshell/mkvirtualenv (&rest args) (apply #'conda-env-mkvirtualenv args))
  (defun eshell/lsvirtualenv () (conda-env-list))
  ;; make completions work
  (conda--make-pcompletions ("activate"))
  (message "Eshell Conda environment support initialized."))

;;;###autoload
(defun conda-env-activate-for-buffer ()
  "Activate the conda environment implied by the current buffer.

This can be set by a buffer-local or project-local variable (e.g. a
`.dir-locals.el` that defines `conda-project-env-path`), or inferred from an
environment YAML file or similar at the project level."
  (interactive)
  (let ((env-path (if (bound-and-true-p conda-project-env-path) conda-project-env-path
                    (let ((inferred-env (conda--infer-env-from-buffer)))
                      (if (not (eql inferred-env nil))
                          (conda-env-name-to-dir inferred-env))))))

    (if (not (eql env-path nil))
        (conda-env-activate env-path)
      (if conda-message-on-environment-switch
          (message "No Conda environment found for <%s>" (buffer-file-name))))))

(defun conda--env-process-exit-message (op env-name &optional callback)
  ;; capture dynamically scoped closures
  (lambda (proc _event)
    (when (memq (process-status proc) '(exit signal))
      (if (zerop (process-exit-status proc))
          (message "Finished %s Conda environment <%s>" op env-name)
        (message "Error while %s Conda environment <%s>" op env-name)))
    (when callback (funcall callback proc _event))))

;;;###autoload
(defun conda-env-yaml-process-for-buffer (&optional remove env-file)
  "Operate on conda environment defined by ENV-FILE, a YAML file.

When ENV-FILE is nil, it attempts to look it up in the parent directories
If not found it raises an error. When ENV-FILE is non-nil, or is found,
and REMOVE is nil, it calls a shell process to update environment,
or to create it if it doesn't yet exist.

If REMOVE is non-nil it attempts to remove the environment if exists,
or reports an error otherwise."
  (interactive "P")
  (let* ((env-file
          (cond ((and env-file (not (f-exists-p env-file)))
                 (user-error "Environment YAML file %s does not exist." env-file))
                (env-file) ; if the file exists, use it!
                ((conda--find-env-yaml
                  (if-let ((filename (buffer-file-name)))
                      (f-dirname filename) default-directory)))))
         (env-name
          (when env-file
            (or (conda--get-name-from-env-yaml env-file)
                (user-error "Could not parse environment name from %s" env-file))))
         (params
          (cond (remove (let* ((candidates (conda-env-candidates))
                               (env-name (and (member env-name candidates) env-name))
                               (env-name (completing-read "Remove Conda environment: "
                                          candidates nil t nil nil env-name)))
                       (list "Removing" "remove" "-y" "-n" env-name)))
                ((member env-name (conda-env-candidates))
                 (list "Updating" "update" "-f" env-file))
                (t (list "Creating" "create" "-f" env-file))))
         (term-buffer
          (unless (and remove (not (y-or-n-p (format "Proceed with removing Conda environment <%s>?"
                                                  (nth 4 params)))))
            (when (and remove (bound-and-true-p conda-env-current-path))
              (conda-env-deactivate))
            (apply #'make-term (concat "conda-env-" (cadr params))
                   (conda--get-executable-path) nil "env" (cdr params)))))
    (when term-buffer
      (message "%s Conda environment <%s>" (car params) env-name)
      (let* ((proc (get-buffer-process term-buffer))
             (current-sentinel (process-sentinel proc)))
        (set-process-sentinel proc
         (conda--env-process-exit-message
          (downcase (car params)) env-name
          current-sentinel)))
      (with-current-buffer term-buffer
        (unless (term-check-proc term-buffer)
          (term-mode)
          (term-line-mode)))
      (switch-to-buffer term-buffer))))

(defcustom conda-env-yaml-default-channels '("conda-forge" "defaults")
  "List of Anaconda channels for new environment YAML files,
used by `conda-env-manage-for-buffer'."
  :type '(list string)
  :group 'conda)

(defcustom conda-env-yaml-default-dependencies '("python" "pip")
  "List of Anaconda package dependencies for new environment YAML files,
used by `conda-env-manage-for-buffer'."
  :type '(list string)
  :group 'conda)

(defcustom conda-env-yaml-default-pip-dependencies '("build" "wheel")
  "List of PIP package dependencies for new environment YAML files,
used by `conda-env-manage-for-buffer'."
  :type '(list string)
  :group 'conda)

(defcustom conda-pip-requirements-filename "requirements.txt"
  "Filename for PIP requirements to be detected when generating new Conda environment YAML files."
  :type 'string
  :safe #'stringp
  :group 'conda)

(make-variable-buffer-local
 'conda-pip-requirements-filename)

(defun conda--pip-requirements-file-expand (dir)
  (let ((path (f-expand conda-pip-requirements-filename dir)))
    (if (f-exists? path) path)))

(defun conda--find-pip-requirements-file (&optional dir)
  "Finds the path of an `conda-env-yaml-pip-requirements-filename' file in DIR,
or one of its parent directories, or else returns nil."
  (let* ((dir (or dir (if-let ((file-name (buffer-file-name))) (f-dirname (f-expand file-name)))))
         (containing-path (and dir (f-traverse-upwards #'conda--pip-requirements-file-expand dir))))
    (and containing-path (conda--pip-requirements-file-expand containing-path))))

(defun conda--choose-new-environment-name (&optional prompt)
  "Prompt for new environment name with PROMPT, ensuring it does not already exist."
  (let ((env-name (read-string (or prompt "Enter name for new conda environment: "))))
    (if (member env-name (conda-env-candidates))
        (conda--choose-new-environment-name (format "Conda environment %s already exists. Try again: " env-name))
      env-name)))

;;;###autoload
(defun conda-env-manage-for-buffer (&optional arg)
  "Edit the Conda environment YAML file implied by the current buffer,
or create, update, remove environments using it.

If an environment YAML file named `conda-env-yaml-base-name' exists, then is used as the reference.

If called without \\[universal-argument] prefix, and an environment file exists, then opens it for editing.
If called without \\[universal-argument] prefix, and no environment file exists, then prompts for an
environment name, and generates a buffer for a new YAML file in the root directory of the current project,
or in the `default-directory'.

The newly generated environment YAML file takes its defaults from these variables:

- `conda-env-yaml-default-channels',
- `conda-env-yaml-default-dependencies',
- `conda-env-yaml-default-pip-dependencies'.

If file named `conda-pip-requirements-filename' exists in one of the parent
directories, then the generated file will be placed next to it instead,
and will be referenced directly for PIP dependencies, instead of using
the `conda-env-yaml-default-pip-dependencies' variable.

If called with one \\[universal-argument] prefix, and an environment file exists,
then updates the environment from the file, or creates it if not yet exists.

If called with two \\[universal-argument] prefix, it prompts for an environment
to be removed. In environment YAML file exists its name is used as default."
  (interactive "P")
  (let* ((dir (if-let ((filename (buffer-file-name)))
                  (f-dirname filename) default-directory))
         (env-file (conda--find-env-yaml dir))
         (project (project-current dir)))
    (cond
     ((and (not env-file) (or (not arg) (and (consp arg) (= (car arg) 4))))
      (let* ((pip-reqs-file (conda--find-pip-requirements-file))
             (env-dir (or (if pip-reqs-file (f-dirname pip-reqs-file))
                          (if project (project-root project)) dir))
             (env-file (f-expand (concat conda-env-yaml-base-name ".yaml") env-dir)))
        (find-file env-file)
        (insert "name: " (conda--choose-new-environment-name))
        (when conda-env-yaml-default-channels
          (insert (mapconcat #'identity
                   (cons "\nchannels:" conda-env-yaml-default-channels)
                   "\n  - ")))
        (when conda-env-yaml-default-dependencies
          (insert (mapconcat #'identity
                   (cons "\ndependencies:" conda-env-yaml-default-dependencies)
                   "\n  - ")))
        (cond
         (pip-reqs-file
          (insert "\n  - pip:\n    - -r " conda-pip-requirements-filename))
         (conda-env-yaml-default-pip-dependencies
          (insert (mapconcat #'identity
                   (cons "\n  - pip:" conda-env-yaml-default-pip-dependencies)
                   "\n    - "))))
        (insert "\n")
        (message "Generated new Conda environment file %s" env-file)))
     ((consp arg)
      (conda-env-yaml-process-for-buffer (<= 14 (car arg)) env-file))
     ;; only interactive universal prefix arguments are expected
     (arg (error "Unsupported prefix argument %s" arg))
     (env-file (find-file env-file)
      (message "Opened Conda environment file %s" env-file)))))

(defun conda--switch-buffer-auto-activate (&rest args)
  "Add Conda environment activation if a buffer has a file, handling ARGS."
  (let ((filename (buffer-file-name)))
    (when filename
      ;; (message "switch-buffer auto-activating on <%s>" filename)
      (with-demoted-errors "Error: %S"
        (conda-env-activate-for-buffer)))))

;;;###autoload
(define-minor-mode conda-env-autoactivate-mode
  "Toggle conda-env-autoactivate mode.

This mode automatically tries to activate a conda environment for the current
buffer."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter nil
  ;; The minor mode bindings.
  :keymap nil
  ;; Kwargs
  :group 'conda
  :global t
  ;; Forms
  (if conda-env-autoactivate-mode ;; already on, now switching off
      (advice-add 'switch-to-buffer :after #'conda--switch-buffer-auto-activate)
    (advice-remove 'switch-to-buffer #'conda--switch-buffer-auto-activate)))

(provide 'conda)

;;; conda.el ends here

;;;; Helper snippet to validate Elisp code
;; (checkdoc)
;; (byte-compile-file (buffer-file-name))
;; (package-buffer-info)
