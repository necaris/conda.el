;;; conda-projectile.el --- Use projectile mode line to display conda env  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  antoine

;; Author: antoine <antoine@antoine-AB350>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The conda env name and the project name very often are the same so instead of clutter the mode line with the same
;; info just add $ char to projectile mode-line part.
;;
;; If the conda env name and projectile match display : Projectile$[project-name:project-type]
;; else : Projectile$conda-env[project-name:project-type]
;;
;; If the name didn't match exactly you can't modify conda-projectile-name-assoc like
;; '(("some-conda-env-name" . "some-projectile-name"))

;;; Code:

(require 'projectile)

(defun conda-projectile-mode-line-setup ()
  (add-to-list 'conda-postactivate-hook #'projectile-update-mode-line)
  (add-to-list 'conda-postdeactivate-hook #'projectile-update-mode-line)
  (setq projectile-mode-line-function #'conda-projectile-mode-line))

(defvar conda-projectile-name-assoc '()
  "AList between conda env name and projectile project name")

(defun conda-projectile--match-env-p ()
  (let ((association (assoc conda-env-current-name conda-projectile-name-assoc))
        (project-name (projectile-project-name)))
    (or
     (equal project-name conda-env-current-name)
     (and association
          (equal project-name (cdr association))))))

(defun conda-projectile-default-mode-line ()
  "Close to projectile-default-mode-line format."
  (let* ((project-name (projectile-project-name))
         (project-type (projectile-project-type)))
    (format "%s%s[%s%s]"
            projectile-mode-line-prefix
            (cond ((conda-projectile--match-env-p) "$")
                  (conda-env-current-name (concat "$" conda-env-current-name))
                  (t ""))
            (or project-name "-")
            (if project-type
                (format ":%s" project-type)
              ""))))


(provide 'conda-projectile)
;;; conda-projectile.el ends here
