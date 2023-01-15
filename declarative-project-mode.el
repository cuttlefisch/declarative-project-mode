;;; declarative-project-mode.el --- Declarative Project mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: January 13, 2023
;; Modified: January 14, 2023
;; Version: 0.0.2
;; Keywords: project management, dependency management, declarative syntax, emacs minor-mode.
;; Homepage: https://github.com/cuttlefisch/declarative-project-mode
;; Package-Requires: ((emacs "26.1"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Declarative Project mode is a minor mode for managing project resources.
;; The mode is triggered by visiting a directory containing a .project file.
;; The .project file should be in json format and contain the following fields
;; "required-resources", "project-deps", "project-local-files", "project-copy-files"
;;
;; Keybindings:
;;   - `C-c i`: Run the install-project command
;;
;;; Code:
(require 'json)
(require 'yaml-mode)
(require 'treemacs)

(defun declarative-project--check-required-resources (project-resources)
  "Warn if any resources labeled required in PROJECT-RESOURCES are missing."
  (when-let ((required-resources (gethash 'required-resources project-resources)))
    (seq-map (lambda (resource)
               (unless (file-exists-p resource)
                 (warn (concat "Missing required resource: " resource))))
             required-resources)))

(defun declarative-project--install-project-dependencies (project-resources)
  "Clone any git dependencies locally in PROJECT-RESOURCES."
  (when-let ((project-deps (gethash 'project-deps project-resources)))
    (seq-map (lambda (dep)
               (unless (file-exists-p (file-name-base dep))
                 (shell-command (concat "git clone " dep))))
             project-deps)))

(defun declarative-project--install-project-local-files (project-resources)
  "Copy over any local files in PROJECT-RESOURCES."
  (when-let ((project-local-files (gethash 'project-local-files project-resources)))
    (when (car project-local-files)
      (seq-map (lambda (file)
                 (copy-file file (concat default-directory file) t))
               project-local-files))))

(defun declarative-project--install-project-copy-files (project-resources)
  "Copy and mutate local files in PROJECT-RESOURCES."
  (when-let ((project-copy-files (gethash 'project-copy-files project-resources)))
    (when (car project-copy-files)
      (seq-map (lambda (file)
                 (let ((new-file-name (concat
                                       (file-name-sans-extension (file-name-nondirectory file))
                                       "_modified"
                                       (file-name-extension (file-name-nondirectory file)))))
                   (copy-file file (concat default-directory new-file-name) t)))
               project-copy-files))))

(defun declarative-project--apply-treemacs-workspaces (project-resources)
  "Add project to any treemacs workspaces listed in PROJECT-RESOURCES."
  (when-let ((project-workspaces (gethash 'treemacs-workspaces project-resources))
             (project-file (gethash 'project-file project-resources)))
    (seq-doseq (workspace project-workspaces)
      (let ((project-name (or (gethash 'project-name project-resources) workspace)))
        (treemacs-do-create-workspace workspace)
        (treemacs-with-workspace (treemacs-find-workspace-by-name workspace)
          (treemacs-do-add-project-to-workspace
           (file-name-directory project-file)
           project-name))))))


(defun declarative-project--install-project ()
  "Step step through project spec & apply any blocks found."
  (interactive)
  (let ((project-file (expand-file-name ".project" default-directory)))
    (when (file-exists-p project-file)
      (with-temp-buffer
        (insert-file-contents project-file)
        (let ((project-resources (or (yaml-parse-string (buffer-string)
                                                        :null-object nil
                                                        :sequence-type 'list)
                                     (json-read-from-string (buffer-string)))))
          (puthash 'project-file project-file project-resources)
          (declarative-project--check-required-resources project-resources)
          (declarative-project--install-project-dependencies project-resources)
          (declarative-project--install-project-local-files project-resources)
          (declarative-project--install-project-copy-files project-resources)
          (declarative-project--apply-treemacs-workspaces project-resources)
          (message "...Finished Installation!"))))))

(define-minor-mode declarative-project-mode
  "Declarative Project mode."
  :lighter " DPM"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c i")
                        'declarative-project--install-project)
            map)
  (if declarative-project-mode
      (declarative-project--install-project)))

(add-hook 'find-file-hook (lambda () (when (string-match-p "/.project$" (buffer-file-name)) (declarative-project-mode 1))))
(provide 'declarative-project-mode)
;;; declarative-project-mode.el ends here
