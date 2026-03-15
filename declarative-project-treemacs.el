;;; declarative-project-treemacs.el --- Treemacs workspace integration for declarative-project-mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: January 14, 2023
;; Modified: March 15, 2026
;; Version: 0.1.0
;; Keywords: project management, treemacs
;; Homepage: https://github.com/cuttlefisch/declarative-project-mode
;; Package-Requires: ((emacs "28.1") (treemacs "2.10") (declarative-project-mode "0.1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Manage treemacs workspaces via distributed declarative .project files.
;;
;; This package allows users to define treemacs workspace assignments in
;; .project files and have them applied via `declarative-project-mode'.
;; The desired state of workspaces is tracked in a central cache inside
;; `user-emacs-directory'.
;;
;; Originally the standalone `treemacs-declarative-workspaces-mode' package,
;; now absorbed into declarative-project-mode.
;;
;;; Code:

(require 'treemacs)
(require 'treemacs-workspaces)
(require 'declarative-project-mode)
(require 'cl-lib)
(require 'seq)

;;; --- Variables ---

(defvar treemacs-declarative-workspaces--desired-state nil
  "List of desired workspaces and project contents from declared workspaces.
Initialized lazily when the mode is enabled.")

(defvar treemacs-declarative-workspaces--cache-file
  (expand-file-name "treemacs-declared-workspaces.el" user-emacs-directory)
  "File path for caching the declared workspace state.")

(defcustom treemacs-declarative-workspaces-autoprune t
  "If non-nil, remove invalid projects from cache on mode init."
  :type 'boolean
  :group 'declarative-project)

;;; --- Internal helpers ---

(defun treemacs-declarative-workspaces--minimal-desired-state ()
  "Return minimal desired state: a single workspace named Default."
  (list (treemacs-workspace->create! :name "Default" :projects '())))

(defun treemacs-declarative-workspaces--ensure-desired-state ()
  "Initialize desired state if not yet set."
  (unless treemacs-declarative-workspaces--desired-state
    (setq treemacs-declarative-workspaces--desired-state
          (treemacs-declarative-workspaces--minimal-desired-state))))

(defun treemacs-declarative-workspaces--workspaces-by-name (name)
  "Return first workspace in desired state named NAME, or nil."
  (seq-find (lambda (ws)
              (string= name (treemacs-workspace->name ws)))
            treemacs-declarative-workspaces--desired-state))

;;; --- Cache persistence ---

(defun treemacs-declarative-workspaces--save-cache ()
  "Write current desired state to cache file."
  (treemacs-declarative-workspaces--ensure-desired-state)
  (with-temp-file treemacs-declarative-workspaces--cache-file
    (let ((standard-output (current-buffer)))
      (insert ";; -*- no-byte-compile: t -*-\n")
      (insert ";; treemacs-declarative-workspaces cache — auto-generated\n")
      (prin1 `(setq treemacs-declarative-workspaces--desired-state
                     ',treemacs-declarative-workspaces--desired-state)))))

(defun treemacs-declarative-workspaces--read-cache ()
  "Read the desired state from the cache file."
  (if (file-exists-p treemacs-declarative-workspaces--cache-file)
      (load treemacs-declarative-workspaces--cache-file nil t t)
    (treemacs-declarative-workspaces--ensure-desired-state)
    (treemacs-declarative-workspaces--save-cache)))

;;; --- Project management ---

(defun treemacs-declarative-workspaces--workspace-memberp (project workspace)
  "Return non-nil if PROJECT is a member of WORKSPACE."
  (member project (treemacs-workspace->projects workspace)))

(defun treemacs-declarative-workspaces--append-project (workspace project)
  "Append PROJECT to WORKSPACE in the desired state."
  (let* ((index (cl-position workspace treemacs-declarative-workspaces--desired-state
                             :test #'equal))
         (new-workspace (copy-sequence workspace))
         (projects (treemacs-workspace->projects new-workspace))
         (new-projects (cl-pushnew project projects
                                   :test #'equal
                                   :key (lambda (pj) (treemacs-project->name pj)))))
    (setf (treemacs-workspace->projects new-workspace) new-projects)
    (when index
      (setf (nth index treemacs-declarative-workspaces--desired-state) new-workspace))))

(defun treemacs-declarative-workspaces--assign-project (project-attrs workspace)
  "Add new project with PROJECT-ATTRS to WORKSPACE in desired state."
  (treemacs-declarative-workspaces--ensure-desired-state)
  (let ((target-workspace (treemacs-declarative-workspaces--workspaces-by-name workspace)))
    (cond
     ((treemacs-workspace-p target-workspace)
      (let ((project (apply #'treemacs-project->create! project-attrs)))
        (unless (treemacs-declarative-workspaces--workspace-memberp project target-workspace)
          (treemacs-declarative-workspaces--append-project target-workspace project))))
     (t
      (cl-pushnew (treemacs-workspace->create!
                   :name workspace
                   :projects (list (apply #'treemacs-project->create! project-attrs)))
                  treemacs-declarative-workspaces--desired-state
                  :test #'equal))))
  (treemacs-declarative-workspaces--save-cache))

(defun treemacs-declarative-workspaces--unassign-project (project workspace)
  "Remove PROJECT from WORKSPACE in desired state."
  (let* ((ws (treemacs-declarative-workspaces--workspaces-by-name workspace))
         (new-projects (cl-remove project
                                  (treemacs-workspace->projects ws)
                                  :test #'string=
                                  :key (lambda (pj) (treemacs-project->name pj)))))
    (when ws
      (setf (treemacs-workspace->projects ws) new-projects)))
  (when treemacs-declarative-workspaces-mode
    (treemacs-declarative-workspaces--override-workspaces))
  (treemacs-declarative-workspaces--save-cache))

(defun treemacs-declarative-workspaces--remove-workspace (workspace)
  "Remove WORKSPACE from the desired state."
  (setq treemacs-declarative-workspaces--desired-state
        (cl-remove (treemacs-declarative-workspaces--workspaces-by-name workspace)
                   treemacs-declarative-workspaces--desired-state
                   :test #'equal))
  (when treemacs-declarative-workspaces-mode
    (treemacs-declarative-workspaces--override-workspaces))
  (treemacs-declarative-workspaces--save-cache))

(defun treemacs-declarative-workspaces--prune-invalid-projects ()
  "Remove invalid projects from cached declared workspaces."
  (dolist (workspace treemacs-declarative-workspaces--desired-state)
    (setf (treemacs-workspace->projects workspace)
          (seq-filter (lambda (project)
                        (file-exists-p (treemacs-project->path project)))
                      (treemacs-workspace->projects workspace)))))

;;; --- Workspace override ---

(defun treemacs-declarative-workspaces--override-workspaces ()
  "Set treemacs workspaces to the desired state."
  (setq treemacs--workspaces treemacs-declarative-workspaces--desired-state))

;;; --- Hook integration ---

(defun treemacs-declarative-workspaces--assign-declared-project (project-resources)
  "Assign a project described by PROJECT-RESOURCES to declared workspaces."
  (when treemacs-declarative-workspaces-mode
    (when-let ((project-workspaces (declarative-project-workspaces project-resources))
               (root-directory (declarative-project-root-directory project-resources)))
      (dolist (workspace project-workspaces)
        (let* ((project-name (or (declarative-project-name project-resources) workspace))
               (project-attrs (list :name project-name
                                    :path root-directory
                                    :path-status 'local-readable
                                    :is-disabled? nil)))
          (treemacs-declarative-workspaces--assign-project project-attrs workspace)))
      (treemacs-declarative-workspaces--override-workspaces)
      (treemacs-declarative-workspaces--save-cache))))

;;; --- Mode definition ---

;;;###autoload
(define-minor-mode treemacs-declarative-workspaces-mode
  "Manage treemacs workspaces via distributed declarative .project files.

This mode allows .project files to declare treemacs workspace assignments.
The desired state of workspaces is tracked in a central cache inside
`user-emacs-directory'.  When enabled, treemacs workspaces are overridden
with the declared desired state.

Note: this mode currently takes full control of the treemacs workspace
list and is still experimental."
  :init-value nil
  :global t
  :group 'declarative-project
  :lighter " TDW"
  (if treemacs-declarative-workspaces-mode
      (progn
        (treemacs-declarative-workspaces--read-cache)
        (when treemacs-declarative-workspaces-autoprune
          (treemacs-declarative-workspaces--prune-invalid-projects))
        (treemacs-declarative-workspaces--override-workspaces)
        (add-hook 'declarative-project--apply-treemacs-workspaces-hook
                  #'treemacs-declarative-workspaces--assign-declared-project)
        (add-hook 'treemacs-switch-workspace-hook
                  #'treemacs-declarative-workspaces--override-workspaces))
    (treemacs-declarative-workspaces--save-cache)
    (remove-hook 'declarative-project--apply-treemacs-workspaces-hook
                 #'treemacs-declarative-workspaces--assign-declared-project)
    (remove-hook 'treemacs-switch-workspace-hook
                 #'treemacs-declarative-workspaces--override-workspaces)))

(provide 'declarative-project-treemacs)
;;; declarative-project-treemacs.el ends here
