;;; declarative-project-mode.el --- Declarative Project mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: January 13, 2023
;; Modified: January 15, 2023
;; Version: 0.0.3
;; Keywords: project management, dependency management, declarative syntax, emacs minor-mode.
;; Homepage: https://github.com/cuttlefisch/declarative-project-mode
;; Package-Requires: ((emacs "26.1") (treemacs "3.0"))
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
;; Declarative Project mode is a minor mode for managing project resources. The
;; mode is triggered by visiting a directory containing a .project file. The
;; .project file should be in yaml or json format and may contain the following
;; fields "project-name", "required-resources", "deps", "local-files",
;; "symlinks", "treemacs-workspaces".
;;
;; Keybindings: - `C-c C-c i`: Run the install-project command when visiting
;; .project file
;;
;;; Code:
(require 'json)
(require 'yaml-mode)
(require 'yaml)
(require 'treemacs)


(defcustom declarative-project--apply-treemacs-workspaces-hook nil
  "Hooks to run whenever the treemacs-workspaces are applied."
  :type 'hook
  :group 'declarative-project-mode-hooks)

(defun declarative-project--check-required-resources (project-resources)
  "Warn if any resources labeled required in PROJECT-RESOURCES are missing."
  (when-let ((required-resources (gethash 'required-resources project-resources)))
    (seq-map (lambda (resource)
               (unless (file-exists-p resource)
                 (warn (concat "Missing required resource: " resource))))
             required-resources)))

(defun declarative-project--install-project-dependencies (project-resources)
  "Clone any git dependencies locally in PROJECT-RESOURCES."
  (when-let ((project-deps (gethash 'deps project-resources)))
    (seq-map (lambda (dep)
               (let ((src (gethash 'src dep))
                     (dest (or (gethash 'dest dep) ""))
                     (args (or (gethash 'args dep) "")))
                 (unless (file-exists-p (file-name-base src))
                   (shell-command (concat "git clone " src " " dest " " args)))))
             project-deps)))

(defun declarative-project--copy-local-files (project-resources)
  "Copy over any local files in PROJECT-RESOURCES."
  (when-let ((local-files (gethash 'local-files project-resources)))
    (seq-map (lambda (file)
               (let* ((src (expand-file-name (gethash 'src file)))
                      (dest (or (gethash 'dest file) (file-name-nondirectory src))))
                 (cond
                 ((file-directory-p src)
                       (unless (file-directory-p (concat default-directory dest))
                        (copy-directory src (concat default-directory dest) t t t)))
                 ((file-exists-p src)
                        (copy-file src (concat default-directory dest) t))
                (t
                   (warn "No such file or directory:\t%s" src)))))
             local-files)))


(defun declarative-project--create-symlinks (project-resources)
  "Copy over any local files in PROJECT-RESOURCES."
  (when-let ((symlinks (gethash 'symlinks project-resources)))
    (seq-map (lambda (link-def)
               (let* ((targ (expand-file-name (gethash 'targ link-def)))
                      (link (or (gethash 'link link-def)
                                (file-name-nondirectory targ))))
                 (if (file-exists-p targ)
                     (progn
                       (make-directory (file-name-parent-directory link) t)
                       (make-symbolic-link targ (concat default-directory link) t))
                   (warn "No such file or directory:\t%s" targ))))
             symlinks)))

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
           project-name))))
    (run-hook-with-args declarative-project--apply-treemacs-workspaces-hook project-resources)))


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
          (declarative-project--copy-local-files project-resources)
          (declarative-project--create-symlinks project-resources)
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
