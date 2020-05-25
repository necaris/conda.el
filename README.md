# conda.el 
[![Build Status](https://github.com/necaris/conda.el/workflows/CI/badge.svg)](https://github.com/necaris/conda.el/actions?query=workflow%3ACI) [![MELPA](http://melpa.milkbox.net/packages/conda-badge.svg)](http://melpa.milkbox.net/#/conda)

Emacs library for working with [conda environments](http://conda.pydata.org/docs/using/envs.html),
largely ported from [virtualenvwrapper.el](https://github.com/porterjamesj/virtualenvwrapper.el).

## what it does

* Makes Python shells, interactive shells, eshell, anaconda-mode, and so on
  aware of your conda environments
* Detects and auto-activates the right conda environment for a particular
  buffer.
  
## running tests

Using [`makem.sh`](https://github.com/alphapapa/makem.sh): 

``` shell
make v=v sandbox=/tmp install-deps=t test-ert
```

make v=v`

## basic usage

* Install [conda](http://conda.pydata.org/docs/index.html) (included if you
  have installed [Anaconda](https://www.continuum.io/downloads) or [Miniconda](http://conda.pydata.org/miniconda.html))
* Install from MELPA (`M-x package-install conda`), or just put `conda.el`
  on your load path.
* Add it to your configuration (e.g. your `.emacs` or `init.el`). Something like
  this should work:

  ```lisp
  (require 'conda)
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  (conda-env-autoactivate-mode t)
  ```

* If your Anaconda installation is anywhere other than the default (`~/.anaconda3`)
  then set the `conda-anaconda-home` custom variable to the installation path. For
  instance, if you installed miniconda via Homebrew on macOS, this should work:
  ```lisp
  (custom-set-variables
   '(conda-anaconda-home "/usr/local/Caskroom/miniconda/base/"))
  ```
  Also, if you have configured your Anaconda environment directory differently from the default
  (i.e. _not_ relative to the Anaconda installation), you additionally need to set the 
  `conda-env-home-directory`. For example, if it's at `~/anaconda3/` (while your main Anaconda
  installation might be at `/opt/anaconda`):
  ```
  (setq conda-env-home-directory (expand-file-name "~/anaconda3/"))
  ```
  Especially if you've changed from the default configuration, be sure to double-check that
  your Conda _environments_ are correctly located. The default subdirectory is `envs`, so
  if your `conda-env-home-directory` is `~/anaconda3` then by default environments will be looked
  for in `~/anaconda3/envs`. You can change this with the `conda-env-subdirectory` setting:
  ```
  (setq 
    conda-env-home-directory (expand-file-name "~/anaconda3/") ;; as in previous example; not required
    conda-env-subdirectory "myenvs")
  ```
  Now environments will be searched for under `~/anaconda3/myenvs`.
  

* Use `M-x conda-env-activate` to activate conda environments and `M-x conda-env-deactivate`
  to deactivate them. You can also use `M-x conda-env-activate-for-buffer` to try
  and detect the correct conda environment for a buffer, or use the `conda-env-autoactivate-mode`
  minor mode to do this automatically (see below for more details).

## what do activating and deactivating actually do?

As with [virtualenvwrapper.el](https://github.com/porterjamesj/virtualenvwrapper.el),
activating a conda environment does:

1. Sets `python-shell-virtualenv-path` to the environment's directory so that
   when you open a new python shell, it is aware of the environment's installed
   packages and modules.
2. The environment's `bin` directory is prepended to the `PATH` environment
   variable and Emacs' internal `exec-path`, so that when a process is launched
   from Emacs it is aware of any executables installed in the virtualenv (e.g
   `py.test`, `pep8`, etc.). This comes in handy for FlyCheck to correctly lint
   your code, or to use `M-! nosetests` to run your tests, and so on.
3. The `VIRTUAL_ENV` environment variable is set to the environment's directory
   so any tools that depend on this variable function correctly (such as
   [jedi](http://tkf.github.io/emacs-jedi/)).
4. `pythonic-activate` is called on the environment to ensure any other Python-
   related code is initialized with the right working path, version of Python,
   and so on.

When you deactivate, all these things are undone. You can safely
modify your `PATH` and `exec-path` while a virtualenv is active and
expect the changes not to be destroyed.

This covers everything except interactive shells, which are
covered in the next section.

## shells

This thing supports two types of interactive shells, the
[eshell](https://www.gnu.org/software/emacs/manual/html_mono/eshell.html)
and the
[interactive subshell](https://www.gnu.org/software/emacs/manual/html_node/emacs/Interactive-Shell.html)
(what you get when you do `M-x shell`).

### interactive shell

Support for interactive shell is turned on by calling `conda-env-initialize-interactive-shell`.
After this is done, whenever you call `shell`, the shell will start in the
correct conda environment. Note that changing the environment in Emacs will not
affect any running shells and vice-versa; they are independent processes.

#### WARNINGS

This feature is a pretty big hack and works by
[advising](https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html)
the `shell` function. This works fine if you haven't otherwise tricked
out or advised it, but if this is the case it may break.

### eshell

Support for eshell is turned on by calling `conda-env-initialize-eshell`. After
doing this, any new eshells you launch will be in the correct environment and
have access to installed executables, etc. The mode also provides a variety of
virtualenvwrapper-like commands that work identically to their bash/zsh
counterparts (described in detail below). Note that in contrast to how
interactive shells work, Eshell shares an environment with Emacs, so if you
activate or deactivate in one, the other is affected as well. Note that this
requires the variable `eshell-modify-global-environment` to be set to true --
running `conda-env-initialize-eshell` causes this to occur.

## command reference

The commands this mode provides are prefixed with `conda-` (right now, the majority
start with `conda-env-` since they deal with environments). All commands can be
called interactively using `M-x`. Many of these commands have also been aliased
without prefixes as eshell functions, so you can call them on the eshell just as
you would in bash or zsh. For example:

```
eshell> activate myenv
eshell> deactivate
```

All will do what would expect.

#### `conda-env-activate`

Prompts for the name of a conda environment and activates it as described above.
Can also be called noninteractively as `(conda-env-activate "<NAME>")`.

#### `conda-env-deactivate`

Deactivates the current conda environment, undoing everything that `conda-env-activate`
did. This can also be called noninteractively as `(conda-env-deactivate)`.

#### `conda-env-list`

List all available conda environments, in a temp buffer.

## useful macros

There is a `conda-with-env` macro, which takes the name of a conda environment and
then any number of forms and executes those forms with that environment active.

Since it's common to want to execute shell commands, there is a convenience macro
`conda-with-env-shell-command`, which takes a string, interpreted as a shell
command, and do exactly what you'd expect. So for example, you can do
`(conda-with-env-shell-command "myenv" "conda install pep8")` to install `pep8`
in the `myenv` conda environment. It can also be called interactively and will
prompt for a command to run if so.

### keybindings

Just like `virtualenvwrapper.el`, no keybindings defined here. Do what you like!

### automatically activating a virtualenv in a particular project

It's also common to want to have an environment automatically activated when you
open a file in a certain project. This can be done with the `conda-env-autoactivate-mode`
minor mode, which will:

  - check for a [per-directory local variable](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
    setting the `conda-project-env-name`
  - search up the directory tree for a file defining a conda environment, such
    as [an `environment.yml` file](http://conda.pydata.org/docs/using/envs.html#share-an-environment),
    and try to activate the named environment

### displaying the currently active environment on the mode line

The name of the currently active conda environment is stored in the variable
`conda-env-current-name`. If you want to have it displayed on your customized
mode line you can just add `(:exec (list conda-env-current-name)))` somewhere
in your `mode-line-format`. If you don't customize your mode line and just want
to have the current virtualenv displayed, you can do:

```lisp
(setq-default mode-line-format (cons '(:exec conda-env-current-name) mode-line-format))
```

### eshell prompt customization

You might also want to have the name of your current conda environment appear on
the eshell prompt. You can do this by a pretty similar mechanism, just include
`conda-env-current-name` in your `eshell-prompt-function` somewhere.

More about customizing the eshell prompt [on the EmacsWiki](http://www.emacswiki.org/emacs/EshellPrompt).

### bugs / comments / contributions

Please open an issue or a PR! I'm happy to pull in contributions or take
suggestions for improvements.

