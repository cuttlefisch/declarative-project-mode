;;; declarative-project-treemacs.el --- Treemacs workspace integration for declarative-project-mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: January 14, 2023
;; Modified: March 20, 2026
;; Version: 0.3.0
;; Keywords: convenience, tools, project
;; Homepage: https://github.com/cuttlefisch/declarative-project-mode
;; Package-Requires: ((emacs "28.1") (treemacs "2.10") (declarative-project-mode "0.3.0"))
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

(defvar declarative-project-treemacs--desired-state nil
  "List of desired workspaces and project contents from declared workspaces.
Initialized lazily when the mode is enabled.")

(defvar declarative-project-treemacs--cache-file
  (expand-file-name "treemacs-declared-workspaces.el" user-emacs-directory)
  "File path for caching the declared workspace state.")

(defcustom declarative-project-treemacs-autoprune t
  "If non-nil, remove invalid projects from cache on mode init."
  :type 'boolean
  :group 'declarative-project
  :package-version '(declarative-project-treemacs . "0.2.0"))

;;; --- Backward compatibility ---

;; Old cache files use `setq' on the old variable name.
(defvaralias 'treemacs-declarative-workspaces--desired-state
  'declarative-project-treemacs--desired-state)

;;; --- Internal helpers ---

(defun declarative-project-treemacs--minimal-desired-state ()
  "Return minimal desired state: a single workspace named Default."
  (list (treemacs-workspace->create! :name "Default" :projects '())))

(defun declarative-project-treemacs--ensure-desired-state ()
  "Initialize desired state if not yet set."
  (unless declarative-project-treemacs--desired-state
    (setq declarative-project-treemacs--desired-state
          (declarative-project-treemacs--minimal-desired-state))))

(defun declarative-project-treemacs--workspaces-by-name (name)
  "Return first workspace in desired state named NAME, or nil."
  (seq-find (lambda (ws)
              (string= name (treemacs-workspace->name ws)))
            declarative-project-treemacs--desired-state))

;;; --- Cache persistence ---

(defun declarative-project-treemacs--normalize-struct (record constructor &rest accessors)
  "Re-create RECORD via CONSTRUCTOR using ACCESSORS to extract fields.
Any accessor that fails (e.g. due to a struct layout change) yields nil.
This ensures cached structs are migrated to the current layout."
  (apply constructor
         (mapcan (lambda (acc)
                   (let* ((keyword (intern (format ":%s" (car acc))))
                          (value (condition-case nil
                                     (funcall (cdr acc) record)
                                   (args-out-of-range nil))))
                     (list keyword value)))
                 accessors)))

(defun declarative-project-treemacs--normalize-cache ()
  "Re-create all workspace/project structs to match current definitions.
Handles cache entries written with older struct layouts that may be
missing fields (e.g. `is-disabled?')."
  (setq declarative-project-treemacs--desired-state
        (mapcar
         (lambda (ws)
           (declarative-project-treemacs--normalize-struct
            ws #'treemacs-workspace->create!
            (cons 'name #'treemacs-workspace->name)
            (cons 'projects
                  (lambda (w)
                    (mapcar
                     (lambda (proj)
                       (declarative-project-treemacs--normalize-struct
                        proj #'treemacs-project->create!
                        (cons 'name #'treemacs-project->name)
                        (cons 'path #'treemacs-project->path)
                        (cons 'path-status #'treemacs-project->path-status)
                        (cons 'is-disabled? #'treemacs-project->is-disabled?)))
                     (treemacs-workspace->projects w))))
            (cons 'is-disabled? #'treemacs-workspace->is-disabled?)))
         declarative-project-treemacs--desired-state)))

(defun declarative-project-treemacs--save-cache ()
  "Write current desired state to cache file."
  (declarative-project-treemacs--ensure-desired-state)
  (with-temp-file declarative-project-treemacs--cache-file
    (let ((standard-output (current-buffer)))
      (insert ";; -*- no-byte-compile: t -*-\n")
      (insert ";; declarative-project-treemacs cache — auto-generated\n")
      (prin1 `(setq declarative-project-treemacs--desired-state
                     ',declarative-project-treemacs--desired-state)))))

(defun declarative-project-treemacs--read-cache ()
  "Read the desired state from the cache file.
After loading, normalizes all structs to the current layout."
  (if (file-exists-p declarative-project-treemacs--cache-file)
      (progn
        (load declarative-project-treemacs--cache-file nil t t)
        (declarative-project-treemacs--normalize-cache))
    (declarative-project-treemacs--ensure-desired-state)
    (declarative-project-treemacs--save-cache)))

;;; --- Project management ---

(defun declarative-project-treemacs--workspace-memberp (project workspace)
  "Return non-nil if PROJECT is a member of WORKSPACE."
  (member project (treemacs-workspace->projects workspace)))

(defun declarative-project-treemacs--append-project (workspace project)
  "Append PROJECT to WORKSPACE in the desired state."
  (let* ((index (cl-position workspace declarative-project-treemacs--desired-state
                             :test #'equal))
         (new-workspace (copy-sequence workspace))
         (projects (treemacs-workspace->projects new-workspace))
         (new-projects (cl-pushnew project projects
                                   :test #'equal
                                   :key (lambda (pj) (treemacs-project->name pj)))))
    (setf (treemacs-workspace->projects new-workspace) new-projects)
    (when index
      (setf (nth index declarative-project-treemacs--desired-state) new-workspace))))

(defun declarative-project-treemacs--assign-project (project-attrs workspace)
  "Add new project with PROJECT-ATTRS to WORKSPACE in desired state."
  (declarative-project-treemacs--ensure-desired-state)
  (let ((target-workspace (declarative-project-treemacs--workspaces-by-name workspace)))
    (cond
     ((treemacs-workspace-p target-workspace)
      (let ((project (apply #'treemacs-project->create! project-attrs)))
        (unless (declarative-project-treemacs--workspace-memberp project target-workspace)
          (declarative-project-treemacs--append-project target-workspace project))))
     (t
      (cl-pushnew (treemacs-workspace->create!
                   :name workspace
                   :projects (list (apply #'treemacs-project->create! project-attrs)))
                  declarative-project-treemacs--desired-state
                  :test #'equal))))
  (declarative-project-treemacs--save-cache))

(defun declarative-project-treemacs--unassign-project (project workspace)
  "Remove PROJECT from WORKSPACE in desired state."
  (when-let ((ws (declarative-project-treemacs--workspaces-by-name workspace)))
    (let ((new-projects (cl-remove project
                                   (treemacs-workspace->projects ws)
                                   :test #'string=
                                   :key (lambda (pj) (treemacs-project->name pj)))))
      (setf (treemacs-workspace->projects ws) new-projects)))
  (when declarative-project-treemacs-mode
    (declarative-project-treemacs--override-workspaces))
  (declarative-project-treemacs--save-cache))

(defun declarative-project-treemacs--remove-workspace (workspace)
  "Remove WORKSPACE from the desired state."
  (setq declarative-project-treemacs--desired-state
        (cl-remove (declarative-project-treemacs--workspaces-by-name workspace)
                   declarative-project-treemacs--desired-state
                   :test #'equal))
  (when declarative-project-treemacs-mode
    (declarative-project-treemacs--override-workspaces))
  (declarative-project-treemacs--save-cache))

(defun declarative-project-treemacs--prune-invalid-projects ()
  "Remove invalid projects from cached declared workspaces."
  (dolist (workspace declarative-project-treemacs--desired-state)
    (setf (treemacs-workspace->projects workspace)
          (seq-filter (lambda (project)
                        (file-exists-p (treemacs-project->path project)))
                      (treemacs-workspace->projects workspace)))))

;;; --- Workspace override ---

(declare-function treemacs-current-workspace "treemacs-workspaces" ())
(declare-function treemacs--invalidate-buffer-project-cache "treemacs-workspaces" ())
(declare-function treemacs-select-window "treemacs" (&optional arg))
(declare-function treemacs-persp--ensure-workspace-exists "treemacs-persp" ())

(defun declarative-project-treemacs--override-workspaces ()
  "Set treemacs workspaces to the desired state.
Also updates treemacs's scope shelf so the current workspace points
to the corresponding object in the desired state list.  When the
current workspace name is not found in the desired state (e.g. a
persp-created workspace), falls back to the first desired workspace.
Prevents treemacs from restoring its persist file by setting the
`:state-is-restored' flag after populating the workspace list."
  (setq treemacs--workspaces declarative-project-treemacs--desired-state)
  ;; Prevent treemacs--maybe-load-workspaces from overwriting our state.
  ;; This mode takes full control of the workspace list, so we
  ;; intentionally suppress treemacs's one-time persist-file restore.
  (put 'treemacs :state-is-restored t)
  ;; Now safe to call treemacs-current-workspace — the lazy-load is a no-op.
  (condition-case nil
      (let* ((current (treemacs-current-workspace))
             (name (and current (treemacs-workspace->name current)))
             (target (or (and name (declarative-project-treemacs--workspaces-by-name name))
                         (car declarative-project-treemacs--desired-state))))
        (when (and target (not (eq current target)))
          (setf (treemacs-current-workspace) target)
          (treemacs--invalidate-buffer-project-cache)))
    (error nil))
  ;; Re-render the treemacs buffer so it reflects the updated workspace.
  (when (treemacs-get-local-buffer)
    (treemacs--consolidate-projects)))

(defun declarative-project-treemacs--on-select (_reason)
  "Re-apply declared workspaces after treemacs window is selected.
Called via `treemacs-select-functions'; REASON is ignored."
  (declarative-project-treemacs--override-workspaces))

(defun declarative-project-treemacs--around-exclusive-display (orig-fn)
  "Show treemacs without modifying workspace content when our mode is active.
ORIG-FN is `treemacs-add-and-display-current-project-exclusively'."
  (if declarative-project-treemacs-mode
      (treemacs-select-window)
    (funcall orig-fn)))

(defun declarative-project-treemacs--around-persp-ensure (orig-fn)
  "Use our workspace override instead of persp workspace creation.
ORIG-FN is `treemacs-persp--ensure-workspace-exists'."
  (if declarative-project-treemacs-mode
      (declarative-project-treemacs--override-workspaces)
    (funcall orig-fn)))

;;; --- Hook integration ---

(defun declarative-project-treemacs--assign-declared-project (project-resources)
  "Assign a project described by PROJECT-RESOURCES to declared workspaces."
  (when declarative-project-treemacs-mode
    (when-let ((project-workspaces (declarative-project-workspaces project-resources))
               (root-directory (declarative-project-root-directory project-resources)))
      (dolist (workspace project-workspaces)
        (let* ((project-name (or (declarative-project-name project-resources) workspace))
               (project-attrs (list :name project-name
                                    :path root-directory
                                    :path-status 'local-readable
                                    :is-disabled? nil)))
          (declarative-project-treemacs--assign-project project-attrs workspace)))
      (declarative-project-treemacs--override-workspaces)
      (declarative-project-treemacs--save-cache))))

;;; --- Mode definition ---

;;;###autoload
(define-minor-mode declarative-project-treemacs-mode
  "Manage treemacs workspaces via distributed declarative .project files.

This mode allows .project files to declare treemacs workspace assignments.
The desired state of workspaces is tracked in a central cache inside
`user-emacs-directory'.  When enabled, treemacs workspaces are overridden
with the declared desired state.

Note: this mode takes full control of the treemacs workspace list.
It is incompatible with `treemacs-project-follow-mode' (disabled
automatically).  It also overrides
`treemacs-add-and-display-current-project-exclusively' (used by
Doom's `+treemacs/toggle') and suppresses `treemacs-persp' workspace
creation when active."
  :init-value nil
  :global t
  :group 'declarative-project
  :lighter " TDW"
  (if declarative-project-treemacs-mode
      (progn
        ;; treemacs-project-follow-mode replaces workspace content with the
        ;; current project on an idle timer, which conflicts with our
        ;; workspace ownership.  Disable it when this mode is active.
        (when (bound-and-true-p treemacs-project-follow-mode)
          (treemacs-project-follow-mode -1)
          (message "declarative-project-treemacs-mode: disabled treemacs-project-follow-mode (incompatible)"))
        (advice-add 'treemacs-add-and-display-current-project-exclusively
                    :around #'declarative-project-treemacs--around-exclusive-display)
        (when (fboundp 'treemacs-persp--ensure-workspace-exists)
          (advice-add 'treemacs-persp--ensure-workspace-exists
                      :around #'declarative-project-treemacs--around-persp-ensure))
        (declarative-project-treemacs--read-cache)
        (when declarative-project-treemacs-autoprune
          (declarative-project-treemacs--prune-invalid-projects))
        (declarative-project-treemacs--override-workspaces)
        (add-hook 'declarative-project--apply-treemacs-workspaces-hook
                  #'declarative-project-treemacs--assign-declared-project)
        (add-hook 'treemacs-switch-workspace-hook
                  #'declarative-project-treemacs--override-workspaces)
        (add-hook 'treemacs-select-functions
                  #'declarative-project-treemacs--on-select))
    (declarative-project-treemacs--save-cache)
    (advice-remove 'treemacs-add-and-display-current-project-exclusively
                   #'declarative-project-treemacs--around-exclusive-display)
    (when (fboundp 'treemacs-persp--ensure-workspace-exists)
      (advice-remove 'treemacs-persp--ensure-workspace-exists
                     #'declarative-project-treemacs--around-persp-ensure))
    (remove-hook 'declarative-project--apply-treemacs-workspaces-hook
                 #'declarative-project-treemacs--assign-declared-project)
    (remove-hook 'treemacs-switch-workspace-hook
                 #'declarative-project-treemacs--override-workspaces)
    (remove-hook 'treemacs-select-functions
                 #'declarative-project-treemacs--on-select)))

(defalias 'treemacs-declarative-workspaces-mode #'declarative-project-treemacs-mode)

(provide 'declarative-project-treemacs)
;;; declarative-project-treemacs.el ends here
