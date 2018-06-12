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

Default automatic integration with `projectile`_ is available since 0.2.0.

Pyenv mode does...
~~~~~~~~~~~~~~~~~~

* Setup ``PYENV_VERSION`` environment variable and
  ``python-shell-virtualenv-path`` custom variable based on user input or
  switching to a projectile project.

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

.. code:: lisp

    (pyenv-mode)

This is enough to turn on ``pyenv-mode`` 's for all ``python-mode`` buffers. If
you are using Projectile, switching to a Python project will setup its Pyenv
Python version for you using either the project's ``.python-version`` file or
the project's name.

If you are not using Projectile, you will have to activate the appropriate
Python version like the following while visiting a ``python`` mode buffer::

    M-x pyenv-mode-set

Advanced Usage
--------------

If you don't want to use the global ``pyenv-mode`` for whatever reason, you can
still activate ``pyenv-mode``, but you have to use the local mode instead::

.. code:: lisp

    (add-hook 'python-mode-hook 'pyenv-local-mode)

Now you are available to specify pyenv python version::

    M-x pyenv-mode-set

So now when you run inferior python with::

    M-x run-python

process will start inside specified python installation.  You can
unset current version with::

    M-x pyenv-mode-unset

FAQ
---

**Q:** My project has multiple ``.python-version`` files in different
subdirectories, when I switch to a different subproject, ``pyenv-mode`` didn't
switch to a different Python version for me, how come?
**A:** By default Projectile only recognizes a couple types of projects by looking
at specific version control system directories or common configuration files.
You can either put an empty ``.projectile`` file in the directory or `register a
new project type`_.

Goodies
-------

When you set python version with ``pyenv-mode`` following changes
happens automatically

* compile commands use proper python version and environment
* flycheck_ perform syntax checking according to python version you use
* anaconda-mode_ search completions, definitions and references in chosen environment

. _python.el: http://repo.or.cz/w/emacs.git/blob_plain/master:/lisp/progmodes/python.el
.. _pyenv: https://github.com/yyuu/pyenv
.. _python-django: https://github.com/fgallina/python-django.el
.. _Melpa: https://melpa.org
.. _flycheck: https://github.com/flycheck/flycheck
.. _anaconda-mode: https://github.com/proofit404/anaconda-mode
.. _projectile: https://github.com/bbatsov/projectile
.. _register a new project type: http://projectile.readthedocs.io/en/latest/configuration/#adding-custom-project-types
