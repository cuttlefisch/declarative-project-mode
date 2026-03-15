;;; declarative-project-mode.el --- Declarative Project mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: January 13, 2023
;; Modified: January 15, 2023
;; Version: 0.1.0
;; Keywords: project management, dependency management, declarative syntax, emacs minor-mode.
;; Homepage: https://github.com/cuttlefisch/declarative-project-mode
;; Package-Requires: ((emacs "28.1") (yaml "0.5.1"))
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
;; Keybindings: - `C-c C-c i': Run the install-project command when visiting
;; .project file
;;
;;; Code:
(require 'json)
(require 'yaml)

(declare-function treemacs-do-create-workspace "treemacs-workspaces" (&optional name))
(declare-function treemacs-find-workspace-by-name "treemacs-workspaces" (name))
(declare-function treemacs-do-add-project-to-workspace "treemacs-workspaces" (path name))

;;; --- Customization ---

(defgroup declarative-project nil
  "Declarative project resource management."
  :group 'tools
  :prefix "declarative-project-")

(defcustom declarative-project-auto-install nil
  "If non-nil, automatically run installation when mode activates."
  :type 'boolean
  :group 'declarative-project)

;;; --- Hooks ---

(defvar declarative-project--apply-treemacs-workspaces-hook nil
  "Hook run when applying treemacs workspaces.
Functions receive a single argument: the project-resources hash table.")

;;; --- Accessor functions ---

(defun declarative-project-workspaces (project-resources)
  "Return the treemacs-workspaces list from PROJECT-RESOURCES."
  (gethash 'treemacs-workspaces project-resources))

(defun declarative-project-root-directory (project-resources)
  "Return the project root directory from PROJECT-RESOURCES."
  (or (gethash 'project-root project-resources)
      (when-let ((pf (gethash 'project-file project-resources)))
        (file-name-directory pf))))

(defun declarative-project-name (project-resources)
  "Return the project name from PROJECT-RESOURCES."
  (gethash 'project-name project-resources))

;;; --- Core functions ---

(defun declarative-project--check-required-resources (project-resources)
  "Warn if any resources labeled required in PROJECT-RESOURCES are missing."
  (when-let ((required-resources (gethash 'required-resources project-resources)))
    (dolist (resource required-resources)
      (unless (file-exists-p resource)
        (warn "Missing required resource: %s" resource)))))

(defun declarative-project--install-project-dependencies (project-resources)
  "Clone any git dependencies locally in PROJECT-RESOURCES."
  (when-let ((project-deps (gethash 'deps project-resources)))
    (dolist (dep project-deps)
      (let* ((src (gethash 'src dep))
             (dest (or (gethash 'dest dep) ""))
             (args (or (gethash 'args dep) ""))
             (effective-dest (if (string-empty-p dest)
                                 (file-name-sans-extension
                                  (file-name-nondirectory src))
                               dest)))
        (unless (file-exists-p (expand-file-name effective-dest))
          (message "Cloning %s..." src)
          (let ((exit-code
                 (apply #'call-process "git" nil nil nil "clone"
                        (append (unless (string-empty-p args)
                                  (split-string-and-unquote args))
                                (list src)
                                (unless (string-empty-p dest) (list dest))))))
            (unless (zerop exit-code)
              (warn "git clone %s failed with exit code %d" src exit-code))))))))

(defun declarative-project--copy-local-files (project-resources)
  "Copy over any local files in PROJECT-RESOURCES."
  (when-let ((local-files (gethash 'local-files project-resources)))
    (dolist (file local-files)
      (let* ((src (expand-file-name (gethash 'src file)))
             (dest (or (gethash 'dest file) (file-name-nondirectory src)))
             (full-dest (expand-file-name dest)))
        (cond
         ((file-directory-p src)
          (unless (file-directory-p full-dest)
            (message "Copying directory %s..." src)
            (copy-directory src full-dest t t t)))
         ((file-exists-p src)
          (message "Copying file %s..." src)
          (copy-file src full-dest t))
         (t
          (warn "No such file or directory:\t%s" src)))))))

(defun declarative-project--create-symlinks (project-resources)
  "Create symlinks defined in PROJECT-RESOURCES."
  (when-let ((symlinks (gethash 'symlinks project-resources)))
    (dolist (link-def symlinks)
      (let* ((targ (expand-file-name (gethash 'targ link-def)))
             (link (or (gethash 'link link-def)
                       (file-name-nondirectory targ)))
             (full-link (expand-file-name link)))
        (if (not (file-exists-p targ))
            (warn "No such file or directory:\t%s" targ)
          (make-directory (file-name-parent-directory full-link) t)
          (message "Creating symlink %s -> %s" link targ)
          (make-symbolic-link targ full-link t))))))

(defun declarative-project--apply-treemacs-workspaces (project-resources)
  "Add project to any treemacs workspaces listed in PROJECT-RESOURCES."
  (when-let ((project-workspaces (gethash 'treemacs-workspaces project-resources))
             (root-dir (declarative-project-root-directory project-resources)))
    (run-hook-with-args 'declarative-project--apply-treemacs-workspaces-hook
                        project-resources)
    (when (featurep 'treemacs)
      (dolist (workspace project-workspaces)
        (let ((project-name (or (gethash 'project-name project-resources) workspace)))
          (message "Adding project to treemacs workspace: %s" workspace)
          (treemacs-do-create-workspace workspace)
          (treemacs-with-workspace (treemacs-find-workspace-by-name workspace)
            (treemacs-do-add-project-to-workspace
             root-dir
             project-name)))))))

(defun declarative-project--parse-project-file (content)
  "Parse CONTENT as YAML or JSON, returning a hash table.
Tries YAML first, then falls back to JSON.  Signals `user-error'
if both parsers fail or if the result is not a hash table."
  (let ((result
         (or (condition-case nil
                 (yaml-parse-string content :null-object nil :sequence-type 'list)
               (error nil))
             (condition-case nil
                 (json-parse-string content :null-object nil :array-type 'list)
               (error nil)))))
    (if (hash-table-p result)
        result
      (user-error "Failed to parse .project file as YAML or JSON"))))

(defun declarative-project--install-from-content (content project-dir &optional extra-keys)
  "Parse CONTENT (YAML/JSON) and run the install pipeline rooted at PROJECT-DIR.
EXTRA-KEYS is an alist of additional keys to set in the resource hash."
  (let* ((default-directory (file-name-as-directory (expand-file-name project-dir)))
         (project-resources (declarative-project--parse-project-file content)))
    (puthash 'project-root default-directory project-resources)
    (dolist (pair extra-keys)
      (puthash (car pair) (cdr pair) project-resources))
    (declarative-project--check-required-resources project-resources)
    (declarative-project--install-project-dependencies project-resources)
    (declarative-project--copy-local-files project-resources)
    (declarative-project--create-symlinks project-resources)
    (declarative-project--apply-treemacs-workspaces project-resources)
    (message "...Finished Installation!")
    project-resources))

(defun declarative-project--install-project ()
  "Step through project spec and apply any blocks found."
  (interactive)
  (let ((project-file (expand-file-name ".project" default-directory)))
    (unless (file-exists-p project-file)
      (user-error "No .project file found in %s" default-directory))
    (message "Installing project from %s..." project-file)
    (declarative-project--install-from-content
     (with-temp-buffer
       (insert-file-contents project-file)
       (buffer-string))
     (file-name-directory project-file)
     (list (cons 'project-file project-file)))))

;;; --- Mode definition ---

(define-minor-mode declarative-project-mode
  "Declarative Project mode."
  :lighter " DPM"
  :group 'declarative-project
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c i")
                        #'declarative-project--install-project)
            map)
  (when (and declarative-project-mode
             declarative-project-auto-install)
    (declarative-project--install-project)))

(defun declarative-project--maybe-enable ()
  "Enable `declarative-project-mode' if visiting a .project file."
  (when-let ((file (buffer-file-name)))
    (when (string-match-p "/\\.project\\'" file)
      (declarative-project-mode 1))))

(add-hook 'find-file-hook #'declarative-project--maybe-enable)

(provide 'declarative-project-mode)
;;; declarative-project-mode.el ends here
