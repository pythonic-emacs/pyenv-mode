# Pyenv mode

Integrate Fabián E. Gallina [python.el][1] with [pyenv][2] tool.  This
allow packages witch already use python.el (like
[python-django.el][3]) got pyenv virtual environments support
out-of-the-box.  Also comint shell, compile commands and pdb debugger
will use proper python version.

### Pyenv mode does...

* Setup `PYENV_VERSION` environment variable and
  `python-shell-virtualenv-path` custom variable based on user input

### Pyenv mode doesn't...

* Override your `exec-path`
* Run external shell scripts
* Manage your pyenv installation
* Deal with virtualenvwrapper

## Installation

You can simply install package from [Melpa][4]

    M-x package-install RET pyenv-mode

## Usage

Add following block to your emacs configuration

```lisp
(pyenv-mode)
```

Now you are available to specify pyenv python version

    M-x pyenv-mode-set-version

So now when you run inferior python with

    M-x run-python

process will start inside specified python installation.  You can
unset current version with

    M-x pyenv-mode-unset-version

[1]: http://repo.or.cz/w/emacs.git/blob_plain/master:/lisp/progmodes/python.el
[2]: https://github.com/yyuu/pyenv
[3]: https://github.com/fgallina/python-django.el
[4]: http://melpa.milkbox.net
