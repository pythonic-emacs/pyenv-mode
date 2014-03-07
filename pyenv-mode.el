;;; pyenv-mode.el --- Integrate pyenv-virtualenv with python-mode

;; Copyright (C) 2014 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/pyenv-mode
;; Version: 0.0.1

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

;; Add following block to your Emacs configuration
;;
;;     (add-hook 'python-mode-hook 'pyenv-mode)
;;
;; Now you are available to specify pyenv virtualenv environment
;;
;;     M-x pyenv-mode-activate
;;
;; So now when you run inferior python with
;;
;;     M-x run-python
;;
;; process will start inside specified environment.  You can unset
;; current environment with
;;
;;     M-x pyenv-mode-deactivate

;;; Code:

(require 'python)

(defun pyenv-mode-root ()
  "Find pyenv installation path."
  (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv root")))

(defun pyenv-mode-virtualenvs ()
  "List virtual environments created with pyenv."
  (let ((virtualenvs (shell-command-to-string "pyenv virtualenvs --bare")))
    (split-string virtualenvs)))

(defun pyenv-mode-read-version ()
  "Read virtual environment from user input."
  (concat (pyenv-mode-root) "/versions/"
          (completing-read "Pyenv: " (pyenv-mode-virtualenvs))))

;;;###autoload
(defun pyenv-mode-activate ()
  "Set `python-shell-virtualenv-path' to some pyenv virtualenv."
  (interactive)
  (setq python-shell-virtualenv-path (pyenv-mode-read-version)))

;;;###autoload
(defun pyenv-mode-deactivate ()
  "Unset `python-shell-virtualenv-path'."
  (interactive)
  (setq python-shell-virtualenv-path nil))

(provide 'pyenv-mode)

;;; pyenv-mode.el ends here
