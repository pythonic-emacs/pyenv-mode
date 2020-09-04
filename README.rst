.. |melpa| image:: https://melpa.org/packages/pyenv-mode-badge.svg
    :target: https://melpa.org/#/pyenv-mode
    :alt: Melpa

==========
Pyenv mode
==========

|melpa|

Pyenv mode integrates Fabián E. Gallina's `python.el`_ with the pyenv_ tool.
This gives packages which already use python.el (like python-django_)
pyenv virtual environment support out-of-the-box.

Pyenv mode does...
~~~~~~~~~~~~~~~~~~

* Setup the ``PYENV_VERSION`` environment variable and
  ``python-shell-virtualenv-path`` custom variable based on user input

Pyenv mode doesn't...
~~~~~~~~~~~~~~~~~~~~~

* Override your ``exec-path``
* Run external shell scripts
* Manage your pyenv installation
* Deal with virtualenvwrapper

Installation
------------

You can simply install the package from Melpa_::

    M-x package-install RET pyenv-mode

Usage
-----

Add following block to your Emacs configuration:

.. code:: lisp

    (pyenv-mode)

Now you can specify the pyenv Python version::

    M-x pyenv-mode-set

So now when you run inferior Python with::

    M-x run-python

The process will start inside the specified Python installation.  You can
unset the current version with::

    M-x pyenv-mode-unset

Goodies
-------

When you set the Python version with ``pyenv-mode``, the following changes
happen automatically:

* compile commands use proper Python version and environment
* flycheck_ performs syntax checking according to Python version you use
* anaconda-mode_ search completions, definitions and references respect the chosen environment

Projectile integration
``````````````````````

You can switch Python versions together with your current project.  Drop
the following lines into your Emacs init file.  When you use projectile switch
project with the ``C-c p p`` key binding ``pyenv-mode`` will activate the
environment matched to the project's name.

.. code:: lisp

    (require 'pyenv-mode)

    (defun projectile-pyenv-mode-set ()
      "Set pyenv version matching project name."
      (let ((project (projectile-project-name)))
        (if (member project (pyenv-mode-versions))
            (pyenv-mode-set project)
          (pyenv-mode-unset))))

    (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

.. _python.el: http://repo.or.cz/w/emacs.git/blob_plain/master:/lisp/progmodes/python.el
.. _pyenv: https://github.com/yyuu/pyenv
.. _python-django: https://github.com/fgallina/python-django.el
.. _Melpa: https://melpa.org
.. _flycheck: https://github.com/flycheck/flycheck
.. _anaconda-mode: https://github.com/proofit404/anaconda-mode
.. _projectile: https://github.com/bbatsov/projectile
