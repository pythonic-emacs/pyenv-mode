;;; pyenv-mode.el --- Integrate pyenv with python-mode

;; Copyright (C) 2014-2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/pyenv-mode
;; Version: 0.2.0
;; Package-Requires: ((pythonic "0.1.0") (f "0.14"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'f)
(require 'pythonic)

(defgroup pyenv nil
  "Pyenv virtualenv integration with python mode."
  :group 'languages)

(defcustom pyenv-mode-mode-line-format
  '(:eval
    (when (pyenv-mode-version)
      (concat "Pyenv:" (pyenv-mode-version) " ")))
  "How `pyenv-mode' will indicate the current python version in the mode line."
  :group 'pyenv)

(defun pyenv-mode-version ()
  "Return currently active pyenv version."
  (getenv "PYENV_VERSION"))

(defun pyenv-mode-root ()
  "Pyenv installation path."
  (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv root")))

(defun pyenv-mode-full-path (version)
  "Return full path for VERSION."
  (unless (string= version "system")
    (concat (pyenv-mode-root) "/versions/" version)))

(defun pyenv-mode-versions ()
  "List installed python versions."
  (let ((versions (shell-command-to-string "pyenv versions --bare")))
    (cons "system" (split-string versions))))

(defun pyenv-mode-read-version ()
  "Read virtual environment from user input."
  (completing-read "Pyenv: " (pyenv-mode-versions)))

(defun pyenv-python-version-file-exists-p (dir)
  "Return t if a .python-version file found under DIR."
  (f-exists? (f-expand ".python-version" dir)))

(defun pyenv-detect-project-python-version ()
  "Returns the Python version found for a project.

Looks at `projectile-project-root' first, if a .python-version
file is found, returns its content.

Otherwise, `projectile-project-name' if it is an installed Python
version.

nil if none of the above is true.
"
  (let* ((project-root (ignore-errors (projectile-project-root)))
         (version-file (and project-root
                            (f-join project-root ".python-version")))
         (version-file-version (and project-root
                                    (f-exists? version-file)
                                    (f-readable? version-file)
                                    (string-trim
                                     (f-read-text version-file 'utf-8))))
         (project-name (projectile-project-name)))
    (cond ((member version-file-version (pyenv-mode-versions))
           version-file-version)
          ((member project-name (pyenv-mode-versions))
           project-name)
          (t nil))))

(defun pyenv-set-project-python-version ()
  "Set Python version when opening a project."
  (let ((python-version (pyenv-detect-project-python-version)))
    (if python-version
        (pyenv-mode-set python-version)
      (pyenv-mode-unset))))

;;;###autoload
(defun pyenv-mode-set (version)
  "Set python shell VERSION."
  (interactive (list (pyenv-mode-read-version)))
  (pythonic-activate (pyenv-mode-full-path version))
  (setenv "PYENV_VERSION" version)
  (force-mode-line-update))

;;;###autoload
(defun pyenv-mode-unset ()
  "Unset python shell version."
  (interactive)
  (pythonic-deactivate)
  (setenv "PYENV_VERSION")
  (force-mode-line-update))

(defvar pyenv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'pyenv-mode-set)
    (define-key map (kbd "C-c C-u") 'pyenv-mode-unset)
    map)
  "Keymap for pyenv-mode.")

;;;###autoload
(define-minor-mode pyenv-local-mode
  "Minor mode for pyenv interaction.

\\{pyenv-mode-map}"
  :lighter ""
  :keymap pyenv-mode-map
  :group 'pyenv
  (if pyenv-local-mode
      (add-to-list 'mode-line-misc-info pyenv-mode-mode-line-format)
    (setq mode-line-misc-info
          (delete pyenv-mode-mode-line-format mode-line-misc-info))))

(defun pyenv-local-mode-turn-on ()
  "Turn on `pyenv-local-mode' if the buffer's major mode is`python-mode'."
  (when (derived-mode-p 'python-mode)
    (pyenv-local-mode)))

;;;###autoload
(define-globalized-minor-mode pyenv-mode pyenv-local-mode
  pyenv-local-mode-turn-on
  :group 'pyenv
  :after-hook
  (if pyenv-mode
      (with-eval-after-load 'projectile
        (add-hook 'projectile-after-switch-project-hook #'pyenv-set-project-python-version))
    (with-eval-after-load 'projectile
      (remove-hook 'projectile-after-switch-project-hook #'pyenv-set-project-python-version))))

(provide 'pyenv-mode)

;;; pyenv-mode.el ends here
