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
;; fields "name", "required-resources", "deps", "local-files",
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

(cl-defstruct declarative-project
  (name                 ""      :type string)
  (root-directory       ""      :type string)
  (required-resources   '()     :type list)
  (deps                 '()     :type list)
  (local-files          '()     :type list)
  (symlinks             '()     :type list)
  (agenda-file          ""      :type string)
  (workspaces           '()     :type list))

(defun declarative-project--make-declarative-project-with-defaults (&rest project-attrs)
  "Create a declarative-project with PROJECT-ATTRS."
  (message "Entered make-declarative-project-with-defaults")
  (pp project-attrs)
  (message (format "project-attrs:\n %s \n\n" project-attrs))
  (apply 'make-declarative-project project-attrs))

(defcustom declarative-project--apply-treemacs-workspaces-hook nil
  "Hooks to run whenever the treemacs-workspaces are applied."
  :type 'hook
  :group 'declarative-project-mode-hooks)


(defun declarative-project--check-required-resources (project)
  "Warn if any resources labeled required in PROJECT are missing."
  (when-let ((required-resources (declarative-project-required-resources project)))
    (seq-map (lambda (resource)
               (unless (file-exists-p resource)
                 (warn (concat "Missing required resource: " resource))))
             required-resources)))

(defun declarative-project--install-project-dependencies (project)
  "Clone any git dependencies locally in PROJECT."
  (when-let ((project-deps (declarative-project-deps project)))
    (seq-map (lambda (dep)
               (let ((src (gethash 'src dep))
                     (dest (or (gethash 'dest dep) ""))
                     (args (or (gethash 'args dep) ""))
                     (root-dir (declarative-project-root-directory project)))
                 ;; Clone any git dependency unless destination already
                 ;; exists.
                 (cond
                  ((file-exists-p (file-name-base dest))
                   (warn "Desintation for %s already exists:\t%s" src dest))
                  (t
                   (shell-command (concat "git clone " src
                                          " " (concat root-dir "/" dest)
                                          " " args))))))
             project-deps)))

(defun declarative-project--copy-local-files (project)
  "Copy over any local files in PROJECT."
  (when-let ((local-files (declarative-project-local-files project)))
    (seq-map
     (lambda (file)
       (let* ((src (expand-file-name (gethash 'src file)))
              (dest (or (gethash 'dest file)
                        (file-name-nondirectory src))))
         (cond
          ((file-directory-p src)
           (unless (file-directory-p
                    (concat (declarative-project-root-directory project) dest))
             (copy-directory src
                             (concat (declarative-project-root-directory project) dest)
                             t t t)))
          ((file-exists-p src)
           (copy-file src (concat (declarative-project-root-directory project) dest)
                      t))
          (t
           (warn "No such file or directory:\t%s" src)))))
     local-files)))


(defun declarative-project--create-symlinks (project)
  "Create any symlinks in PROJECT struct."
  (when-let ((symlinks (declarative-project-symlinks project)))
    (seq-map (lambda (link-def)
               (let* ((targ (expand-file-name (gethash 'targ link-def)))
                      (link (or (gethash 'link link-def)
                                (file-name-nondirectory targ)))
                      (root-dir (declarative-project-root-directory project)))
                 (if (file-exists-p targ)
                     (progn
                       (make-directory (concat root-dir "/" (file-name-parent-directory link)) t)
                       (make-symbolic-link targ (concat root-dir "/" link) t))
                   (warn "No such file or directory:\t%s" targ))))
             symlinks)))

(defun declarative-project--apply-treemacs-workspaces (project)
  "Add project to any treemacs workspaces listed in PROJECT."
  (when-let ((workspaces (declarative-project-workspaces project)))
    (seq-map (lambda (workspace)
               (treemacs-do-add-project-to-workspace
                (declarative-project-root-directory project)
                workspace))
             workspaces)
    (run-hooks 'declarative-project--apply-treemacs-workspaces-hook)))

(defun declarative-project--prep-target (project)
  "If user creates the target file, or it exists & is writable
return t, else nil."
  (let ((root-dir (declarative-project-root-directory project)))
    (if (file-writable-p root-dir)
        t
      (if (yes-or-no-p (format "Directory %s does not exist, create it? " root-dir))
          (make-directory root-dir t)
        (message "Installation Aborted")
        nil))))


(defun declarative-project--install-project ()
  "Step step through project spec & apply any blocks found."
  (interactive)
  (let ((project-file (expand-file-name ".project" default-directory)))
    (when (file-exists-p project-file)
      (with-temp-buffer
        (insert-file-contents project-file)
        (let* ((project-resources (or (yaml-parse-string (buffer-string)
                                                         :null-object nil
                                                         :sequence-type 'list)
                                      (json-read-from-string (buffer-string))))
               (project (make-declarative-project
                         :name (gethash 'project-name project-resources)
                         :root-directory (or (gethash 'root-directory project-resources)
                                             (gethash 'project-file project-resources))
                         :required-resources (gethash 'required-resources project-resources)
                         :deps (gethash 'deps project-resources)
                         :local-files (gethash 'local-files project-resources)
                         :symlinks (gethash 'symlinks project-resources)
                         :agenda-file (gethash 'agenda-file project-resources)
                         :workspaces (gethash 'workspaces project-resources))))
          (declarative-project--prep-target project)
          (declarative-project--check-required-resources project)
          (declarative-project--install-project-dependencies project)
          (declarative-project--copy-local-files project)
          (declarative-project--create-symlinks project)
          (declarative-project--apply-treemacs-workspaces project)
          (message "...Finished Installation!"))))))

(define-minor-mode declarative-project-mode
  "Declarative Project mode."
  :lighter " DPM"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c i")
                        'declarative-project--install-project)
            map)
  (if declarative-project-mode
      (message "Declarative Project Mode Enabled!")))

(add-hook 'find-file-hook (lambda ()
                            (when (string-match-p "/.project$" (buffer-file-name))
                              (declarative-project-mode 1))))
(provide 'declarative-project-mode)
;;; declarative-project-mode.el ends here
