.. |melpa| image:: https://melpa.org/packages/pyenv-mode-badge.svg
    :target: https://melpa.org/#/pyenv-mode
    :alt: Melpa

==========
Pyenv mode
==========

|melpa|

Integrate Fabián E. Gallina `python.el`_ with pyenv_ tool.  This allow
packages which already use python.el (like python-django_) got pyenv
virtual environments support out-of-the-box.

Pyenv mode does...
~~~~~~~~~~~~~~~~~~

* Setup ``PYENV_VERSION`` environment variable and
  ``python-shell-virtualenv-path`` custom variable based on user input

Pyenv mode doesn't...
~~~~~~~~~~~~~~~~~~~~~

* Override your ``exec-path``
* Run external shell scripts
* Manage your pyenv installation
* Deal with virtualenvwrapper

Installation
------------

You can simply install package from Melpa_::

    M-x package-install RET pyenv-mode

Usage
-----

Add following block to your emacs configuration

.. code:: lisp

    (pyenv-mode)

Now you are available to specify pyenv python version::

    M-x pyenv-mode-set

So now when you run inferior python with::

    M-x run-python

process will start inside specified python installation.  You can
unset current version with::

    M-x pyenv-mode-unset

Goodies
-------

When you set python version with ``pyenv-mode`` following changes
happens automatically

* compile commands use proper python version and environment
* flycheck_ perform syntax checking according to python version you use
* anaconda-mode_ search completions, definitions and references in chosen environment

Projectile integration
``````````````````````

You can switch python version together with current project.  Drop
following lines into emacs init file.  When use projectile switch
project with ``C-c p p`` key binding ``pyenv-mode`` will activate
environment matched project name.

.. code:: lisp

    (defun projectile-pyenv-mode-set ()
      "Set pyenv version matching project name.
    Version must be already installed."
      (pyenv-mode-set (projectile-project-name)))

    (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

.. _python.el: http://repo.or.cz/w/emacs.git/blob_plain/master:/lisp/progmodes/python.el
.. _pyenv: https://github.com/yyuu/pyenv
.. _python-django: https://github.com/fgallina/python-django.el
.. _Melpa: https://melpa.org
.. _flycheck: https://github.com/flycheck/flycheck
.. _anaconda-mode: https://github.com/proofit404/anaconda-mode
.. _projectile: https://github.com/bbatsov/projectile
