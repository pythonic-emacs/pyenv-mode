;;; pyenv-mode.el --- Integrate pyenv-virtualenv with python-mode

;; Copyright (C) 2014 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/pyenv-mode
;; Version: 0.0.2

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

(defgroup pyenv-mode nil
  "Pyenv virtualenv integration with python mode."
  :group 'languages)

(defcustom pyenv-mode-mode-line-format
  '(python-shell-virtualenv-path
    (:eval (concat "Pyenv:" (file-name-base python-shell-virtualenv-path) " ")))
  "How `pyenv-mode' will indicate the current environment in the mode line."
  :group 'pyenv-mode)

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
  (setq python-shell-virtualenv-path (pyenv-mode-read-version))
  (force-mode-line-update))

;;;###autoload
(defun pyenv-mode-deactivate ()
  "Unset `python-shell-virtualenv-path'."
  (interactive)
  (setq python-shell-virtualenv-path nil)
  (force-mode-line-update))

(defvar pyenv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") 'pyenv-mode-activate)
    (define-key map (kbd "C-c C-d") 'pyenv-mode-deactivate)
    map)
  "Keymap for pyenv-mode.")

;;;###autoload
(define-minor-mode pyenv-mode
  "Minor mode for pyenv interaction.

\\{pyenv-mode-map}"
  :lighter ""
  :keymap pyenv-mode-map
  (if pyenv-mode
      (add-to-list 'mode-line-misc-info pyenv-mode-mode-line-format)
    (setq mode-line-misc-info
          (delete pyenv-mode-mode-line-format mode-line-misc-info))))

(provide 'pyenv-mode)

;;; pyenv-mode.el ends here
