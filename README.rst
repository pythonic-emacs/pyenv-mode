Pyenv mode
==========

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

    M-x pyenv-mode-set-version

So now when you run inferior python with::

    M-x run-python

process will start inside specified python installation.  You can
unset current version with::

    M-x pyenv-mode-unset-version

Goodies
-------

* compile commands use proper python version and environment
* flycheck_ doesn't show syntax error in python2 buffers if you use python3 by default
* anaconda-mode_ make completions, definitions and references search in chosen environment automatically

.. _python.el: http://repo.or.cz/w/emacs.git/blob_plain/master:/lisp/progmodes/python.el
.. _pyenv: https://github.com/yyuu/pyenv
.. _python-django: https://github.com/fgallina/python-django.el
.. _Melpa: http://melpa.milkbox.net
.. _flycheck: https://github.com/flycheck/flycheck
.. _anaconda-mode: https://github.com/proofit404/anaconda-mode
