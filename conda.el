;;; conda.el --- Work with your conda environments

;; Copyright (C) 2016-2020 Rami Chowdhury
;; Author: Rami Chowdhury <rami.chowdhury@gmail.com>
;; URL: http://github.com/necaris/conda.el
;; Version: 0.4
;; Package-Version: 0.4
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
(require 'json)

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
  :type 'directory
  :group 'conda)

(defcustom conda-system-gud-pdb-command-name
  (if (boundp 'gud-pdb-command-name)
      gud-pdb-command-name
    (setq gud-pdb-command-name "python -m pdb"))
  "Whatever `gud-pdb-command-name' is (usually \\[pdb])."
  :type 'string
  :group 'conda)

(defcustom conda-env-home-directory conda-anaconda-home
  "Location of the directory containing the environments directory."
  :type 'directory
  :group 'conda)

(defcustom conda-env-subdirectory "envs"
  "Location of the environments subdirectory relative to `conda-env-home-directory`."
  :type 'string
  :group 'conda)

(defcustom conda-message-on-environment-switch t
  "Whether to message when switching environments. Default true."
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
  "Name of the directory containing metadata. This should be consistent across platforms.")

(defvar conda-env-name-for-buffer nil  ;; placeholder for buffer-local variable
  "Current conda environment for the project.  Should always be buffer-local.")
;; ensure it's considered safe
(put 'conda-env-name-for-buffer 'safe-local-variable 'stringp)

;; internal utility functions

(defvar conda--installed-version nil
  "Cached copy of installed Conda version. Set for the lifetime of the process.")

(defun conda--get-installed-version()
  "Return currently installed Conda version. Cached for the lifetime of the process."
  (if (not (eq conda--installed-version nil))
      conda--installed-version
    (s-with (shell-command-to-string "conda -V")
      (s-trim)
      (s-split " ")
      (cadr)
      (s-split "\\.")
      (mapcar #'string-to-number)
      (vconcat)
      (setq conda--installed-version))))

(defun conda--version>= (v1 v2)
  "Is version vector V1 greater than or equal to V2?"
  (cl-loop for x across v1
           for y across v2
           if (> x y) return nil
           finally return (>= x y)))

(defun conda--supports-json-activator ()
  "Does the installed Conda version support JSON activation? See https://github.com/conda/conda/blob/master/CHANGELOG.md#484-2020-08-06."
  (conda--version>= (conda--get-installed-version) [4 8 4]))

(defun conda--update-env-from-params (params)
  "Update the environment from PARAMS."
  (let ((exports (or (conda-env-params-vars-export params) '())))
    (mapc (lambda (pair)
            (setenv (car pair) (cdr pair)))
          exports)))

(defun conda--set-env-gud-pdb-command-name ()
  "When in a conda environment, call pdb as \\[python -m pdb]."
  (setq gud-pdb-command-name "python -m pdb"))

(defun conda--set-system-gud-pdb-command-name ()
  "Set the system \\[pdb] command."
  (setq gud-pdb-command-name conda-system-gud-pdb-command-name))

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

(defun conda--check-executable ()
  "Verify there is a conda executable available, throwing an error if not."
  (unless (executable-find "conda")
    (error "There doesn't appear to be a conda executable on your exec path.  A
    common cause of problems like this is GUI Emacs not having environment
    variables set up like the shell.  Check out
    https://github.com/purcell/exec-path-from-shell for a robust solution to
    this problem")))

(defun conda--contains-env-yml? (candidate)
  "Does CANDIDATE contain an environment.yml?"
  (f-exists? (f-expand "environment.yml" candidate)))

(defun conda--find-env-yml (dir)
  "Find an environment.yml in DIR or its parent directories."
  ;; TODO: implement an optimized finder with e.g. projectile? Or a series of
  ;; finder functions, that stop at the project root when traversing
  (let ((containing-path (f-traverse-upwards 'conda--contains-env-yml? dir)))
    (if containing-path
        (f-expand "environment.yml" containing-path)
      nil)))

(defun conda--get-name-from-env-yml (filename)
  "Pull the `name` property out of the YAML file at FILENAME."
  ;; TODO: find a better way than slurping it in and using a regex...
  (when filename
    (let ((env-yml-contents (f-read-text filename)))
      (if (string-match "name:[ ]*\\([A-z0-9-_.]+\\)[ ]*$" env-yml-contents)
          (match-string 1 env-yml-contents)
        nil))))

(defun conda--infer-env-from-buffer ()
  "Search up the project tree for an `environment.yml` defining a conda env."
  (let ((filename (buffer-file-name)))
    (when filename
      (conda--get-name-from-env-yml (conda--find-env-yml (f-dirname filename))))))

(cl-defstruct conda-env-params
  "Parameters necessary for (de)activating a Conda environment"
  path
  vars-export
  vars-set
  vars-unset
  scripts-activate
  scripts-deactivate)

(defun conda--get-activation-parameters (env-dir)
  "Return activation values for the environment in ENV-DIR, as a `conda-env-params'
struct. At minimum, this will contain an updated PATH."
  (if (not (conda--supports-json-activator))
      (make-conda-env-params
       :path (concat (conda--get-path-prefix env-dir) path-separator (getenv "PATH")))
    (let* ((cmd (format "conda shell.posix+json activate %s" env-dir))
           (output (shell-command-to-string cmd))
           ;; TODO: use `json-parse-string' on sufficiently recent Emacs
           (result (json-read-from-string output)))
      (make-conda-env-params
       :path (s-join path-separator (alist-get 'PATH (alist-get 'path result)))
       :vars-export (alist-get 'export (alist-get 'vars result))
       :vars-set (alist-get 'set (alist-get 'vars result))
       :vars-unset (alist-get 'unset (alist-get 'vars result))
       :scripts-activate (alist-get 'activate (alist-get 'scripts result))
       :scripts-deactivate  (alist-get 'deactivate (alist-get 'scripts result))))))

(defun conda--get-deactivation-parameters (env-dir)
  "Return activation values for the environment in ENV-DIR, as a `conda-env-params'
struct. At minimum, this will contain an updated PATH."
  (if (not (conda--supports-json-activator))
      (make-conda-env-params
       :path (s-with (getenv "PATH")
               (s-split path-separator)
               (conda-env-stripped-path)
               (s-join path-separator)))
    (let* ((cmd (format "conda shell.posix+json deactivate %s" env-dir))
           (output (shell-command-to-string cmd))
           ;; TODO: use `json-parse-string' on sufficiently recent Emacs
           (result (json-read-from-string output)))
      (make-conda-env-params
       :path (s-join path-separator (alist-get 'PATH (alist-get 'path result)))
       :vars-export (alist-get 'export (alist-get 'vars result))
       :vars-set (alist-get 'set (alist-get 'vars result))
       :vars-unset (alist-get 'unset (alist-get 'vars result))
       :scripts-activate (alist-get 'activate (alist-get 'scripts result))
       :scripts-deactivate  (alist-get 'deactivate (alist-get 'scripts result))))))



(defun conda--get-path-prefix (env-dir)
  "Get a platform-specific path string to utilize the conda env in ENV-DIR.
It's platform specific in that it uses the platform's native path separator.
(NOTE: prefer `conda--get-activation-parameters' to this where possible)."
  (s-trim
   (with-output-to-string
     (let ((conda-anaconda-home-tmp conda-anaconda-home))
       (with-current-buffer standard-output
         (let* ((conda-executable-path
                 (concat (file-name-as-directory conda-anaconda-home-tmp)
                         (file-name-as-directory conda-env-executables-dir)
                         "conda"))
                (command-format-string "%s ..activate \"%s\" \"%s\"")
                (executor (if (eq system-type 'windows-nt) "cmd.exe" "bash"))
                (command (format command-format-string
                                 conda-executable-path
                                 executor
                                 env-dir))
                (return-code (process-file shell-file-name nil '(t nil) nil shell-command-switch command)))
           (unless (= 0 return-code)
             (error (format "Error: executing command \"%s\" produced error code %d" command return-code)))))))))

;; "public" functions

(defun conda-env-clear-history ()
  "Clear the history of conda environments that have been activated."
  (setq conda-env-history nil))

(defun conda-env-default-location ()
  "Default location of the conda environments -- under the Anaconda installation."
  (f-full (concat (file-name-as-directory conda-env-home-directory) conda-env-subdirectory)))

(defun conda-env-name-to-dir (name)
  "Translate NAME to the directory where the environment is located."
  (if (and (string= name "base")
           (conda--env-dir-is-valid conda-anaconda-home))
      (file-name-as-directory (expand-file-name conda-anaconda-home))
    (let* ((default-location (file-name-as-directory (conda-env-default-location)))
           (initial-possibilities (list name (concat default-location name)))
           (possibilities (if (boundp 'venv-location)
                              (if (stringp 'venv-location)
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
  "Strip PATH-OR-PATH-ELEMENTS of anything inserted by the current environment, returning a list of new path elements."
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

;; potentially interactive user-exposed functions

;;;###autoload
(defun conda-env-deactivate ()
  "Deactivate the current conda env."
  (interactive)
  (when (bound-and-true-p conda-env-current-path)
    (run-hooks 'conda-predeactivate-hook)
    (setq python-shell-virtualenv-root nil)
    (let ((params (conda--get-deactivation-parameters conda-env-current-path)))
      (setq exec-path (conda-env-params-path params))
      (setenv "PATH" (s-join path-separator (conda-env-params-path params)))
      (if (not (eq nil (conda-env-params-vars-export params)))
          (conda--update-env-from-params params)
        (progn ;; otherwise we fall back to legacy heuristics
          (setenv "VIRTUAL_ENV" nil)
          (setenv "CONDA_PREFIX" nil))))
    (setq conda-env-current-path nil)
    (setq conda-env-current-name nil)
    (setq eshell-path-env (getenv "PATH"))
    (conda--set-system-gud-pdb-command-name)
    (run-hooks 'conda-postdeactivate-hook)
    (when (called-interactively-p 'interactive)
      (message "conda env deactivated"))))

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
        (let ((params (conda--get-activation-parameters env-path)))
          (setq exec-path (conda-env-params-path params))
          (if (not (eq nil (conda-env-params-vars-export params)))
              (conda--update-env-from-params params)
            (progn ;; otherwise we fall back to legacy heuristics
              (setenv "PATH" (concat path-separator (conda-env-params-path params)))
              (setq eshell-path-env (getenv "PATH"))
              (setenv "VIRTUAL_ENV" env-dir)
              (setenv "CONDA_PREFIX" env-dir))))
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
             ,@forms) ;; evaluate forms
         (if prev-env ;; switch back
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
  (message "Eshell Conda environment support initialized."))

;;;###autoload
(defun conda-env-activate-for-buffer ()
  "Activate the conda environment implied by the current buffer.

This can be set by a buffer-local or project-local variable (e.g. a
`.dir-locals.el` that defines `conda-project-env-path`), or inferred from an
`environment.yml` or similar at the project level."
  (interactive)
  (let ((env-name-path (if (bound-and-true-p conda-project-env-path)
                           conda-project-env-path
                         (conda-env-name-to-dir (conda--infer-env-from-buffer)))))
    (if (not env-name-path)
        (if conda-message-on-environment-switch
            (message "No conda environment for <%s>" (buffer-file-name)))
      (conda-env-activate env-name-path))))

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
