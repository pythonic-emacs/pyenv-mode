# Pyenv mode

Integrate Fabián E. Gallina [python.el][1] with [pyenv-virtualenv][2]
tool. This allow packages witch already use python.el (like
[python-django.el][3]) got pyenv virtual environments support
out-of-the-box.

### Pyenv mode does...

* Setting up special python.el variable `python-shell-virtualenv-path`
  based on user input

### Pyenv mode doesn't...

* Override your `exec-path`
* Change emacs `process-environment`
* Support virtualenv wrapper
* Manage your pyenv installation

## Installation

You can simply install package from [Melpa][4]

    M-x package-install RET pyenv-mode

## Usage

Add following block to your emacs configuration

```lisp
(add-hook 'python-mode-hook 'pyenv-mode)
```

Now you are available to specify pyenv virtualenv environment

    M-x pyenv-mode-activate

So now when you run inferior python with

    M-x run-python

process will start inside specified environment. You can unset current
environment with

    M-x pyenv-mode-deactivate

[1]: http://repo.or.cz/w/emacs.git/blob_plain/master:/lisp/progmodes/python.el
[2]: https://github.com/yyuu/pyenv-virtualenv
[3]: https://github.com/fgallina/python-django.el
[4]: http://melpa.milkbox.net
