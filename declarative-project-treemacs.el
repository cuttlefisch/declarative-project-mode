;;; declarative-project-treemacs.el --- Treemacs workspace integration for declarative-project-mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <system.cuttle@gmail.com>
;; Maintainer: Hayden Stanko <system.cuttle@gmail.com>
;; Created: January 14, 2023
;; Modified: March 21, 2026
;; Version: 0.3.3
;; Keywords: convenience, tools, project
;; Homepage: https://github.com/cuttlefisch/declarative-project-mode
;; Package-Requires: ((emacs "28.1") (treemacs "2.10") (declarative-project-mode "0.3.3"))
;;
;; This file is not part of GNU Emacs.
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

(defvar declarative-project-treemacs--current-workspace-name nil
  "Name of the last-selected workspace, persisted across sessions.")

(defcustom declarative-project-treemacs-cache-file
  (expand-file-name "treemacs-declared-workspaces.el" user-emacs-directory)
  "File path for caching the declared workspace state.
Doom Emacs users may want to set this under `doom-cache-dir'.
Spacemacs users may want to use `spacemacs-cache-directory'."
  :type 'file
  :group 'declarative-project
  :package-version '(declarative-project-treemacs . "0.3.2"))

(defvaralias 'declarative-project-treemacs--cache-file
  'declarative-project-treemacs-cache-file
  "Backward-compat alias for the internal name.")

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
  "Write current desired state and current workspace name to cache file."
  (declarative-project-treemacs--ensure-desired-state)
  (with-temp-file declarative-project-treemacs-cache-file
    (let ((standard-output (current-buffer)))
      (insert ";; -*- no-byte-compile: t -*-\n")
      (insert ";; declarative-project-treemacs cache — auto-generated\n")
      (prin1 `(setq declarative-project-treemacs--desired-state
                     ',declarative-project-treemacs--desired-state))
      (insert "\n")
      (prin1 `(setq declarative-project-treemacs--current-workspace-name
                     ',declarative-project-treemacs--current-workspace-name)))))

(defun declarative-project-treemacs--read-cache ()
  "Read the desired state from the cache file.
After loading, normalizes all structs to the current layout.
If the cache file is missing, corrupted, or empty, falls back to
minimal desired state and re-saves."
  (if (and (file-exists-p declarative-project-treemacs-cache-file)
           (> (file-attribute-size
               (file-attributes declarative-project-treemacs-cache-file))
              0))
      (condition-case err
          (progn
            (load declarative-project-treemacs-cache-file nil t t)
            (declarative-project-treemacs--normalize-cache))
        (error
         (display-warning 'declarative-project
                          (format "Cache file corrupt, resetting: %s"
                                  (error-message-string err)))
         (setq declarative-project-treemacs--desired-state nil)
         (declarative-project-treemacs--ensure-desired-state)
         (declarative-project-treemacs--save-cache)))
    (declarative-project-treemacs--ensure-desired-state)
    (declarative-project-treemacs--save-cache)))

;;;###autoload
(defun declarative-project-treemacs-reset-cache ()
  "Reset the declared workspace cache to a single empty Default workspace.
Use this to clear stale entries accumulated from old .project files
or babel blocks.  After resetting, re-run your declarative-project
installs to repopulate."
  (interactive)
  (setq declarative-project-treemacs--desired-state nil)
  (setq declarative-project-treemacs--current-workspace-name nil)
  (declarative-project-treemacs--ensure-desired-state)
  (declarative-project-treemacs--save-cache)
  (when declarative-project-treemacs-mode
    (declarative-project-treemacs--override-workspaces))
  (message "declarative-project-treemacs: cache reset to Default workspace"))

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
(declare-function treemacs--restore "treemacs-persistence" ())
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
  (declarative-project-treemacs--ensure-desired-state)
  (setq treemacs--workspaces declarative-project-treemacs--desired-state)
  ;; Prevent treemacs--maybe-load-workspaces from overwriting our state.
  ;; This mode takes full control of the workspace list, so we
  ;; intentionally suppress treemacs's one-time persist-file restore.
  (put 'treemacs :state-is-restored t)
  ;; Now safe to call treemacs-current-workspace — the lazy-load is a no-op.
  (condition-case nil
      (let* ((current (treemacs-current-workspace))
             (name (and current (treemacs-workspace->name current)))
             (target (or (and declarative-project-treemacs--current-workspace-name
                              (declarative-project-treemacs--workspaces-by-name
                               declarative-project-treemacs--current-workspace-name))
                         (and name (declarative-project-treemacs--workspaces-by-name name))
                         (car declarative-project-treemacs--desired-state))))
        (when (and target (not (eq current target)))
          (setf (treemacs-current-workspace) target)
          (treemacs--invalidate-buffer-project-cache)))
    (error nil))
  ;; Re-render the treemacs buffer so it reflects the updated workspace.
  (when (treemacs-get-local-buffer)
    (treemacs--consolidate-projects)))

(defun declarative-project-treemacs--sync-workspace-list ()
  "Ensure `treemacs--workspaces' contains our desired state.
Unlike `--override-workspaces', this does NOT change the current
workspace in the scope shelf.  Use this for hooks that fire during
normal operation (perspective switches, treemacs select, workspace
switch) where we want to maintain the workspace list but let
treemacs or the user control which workspace is active."
  (declarative-project-treemacs--ensure-desired-state)
  (setq treemacs--workspaces declarative-project-treemacs--desired-state)
  (put 'treemacs :state-is-restored t))

(defun declarative-project-treemacs--on-select (_reason)
  "Re-apply declared workspace list after treemacs window is selected.
Called via `treemacs-select-functions'; REASON is ignored."
  (declarative-project-treemacs--sync-workspace-list))

(defun declarative-project-treemacs--on-workspace-switch ()
  "Record current workspace name and sync workspace list.
Used on `treemacs-switch-workspace-hook' to persist the user's
workspace selection across sessions."
  (declarative-project-treemacs--sync-workspace-list)
  (condition-case nil
      (let ((ws (treemacs-current-workspace)))
        (when ws
          (setq declarative-project-treemacs--current-workspace-name
                (treemacs-workspace->name ws))
          (declarative-project-treemacs--save-cache)))
    (error nil)))

(defun declarative-project-treemacs--around-exclusive-display (orig-fn)
  "Show treemacs without modifying workspace content when our mode is active.
ORIG-FN is `treemacs-add-and-display-current-project-exclusively'."
  (if declarative-project-treemacs-mode
      (treemacs-select-window)
    (funcall orig-fn)))

(defun declarative-project-treemacs--around-persp-ensure (orig-fn)
  "Suppress persp workspace creation; maintain our workspace list instead.
ORIG-FN is `treemacs-persp--ensure-workspace-exists'.
Only syncs the workspace list — does not change the current workspace,
so perspective switches don't reset the user's workspace selection."
  (if declarative-project-treemacs-mode
      (declarative-project-treemacs--sync-workspace-list)
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

;;; --- Persistence defense ---

(defun declarative-project-treemacs--after-treemacs-restore (&rest _)
  "Re-apply desired state after treemacs restores from persist file.
Safety net: if `treemacs--restore' fires despite the pre-emption flag
\(e.g. mode enabled after treemacs started, or another package forces
a restore), this advice ensures our desired state takes precedence."
  (when declarative-project-treemacs-mode
    (declarative-project-treemacs--override-workspaces)))

(defun declarative-project-treemacs--sync-before-persist ()
  "Ensure treemacs persists our desired state, not stale data.
Runs on `kill-emacs-hook' at depth -90 (before treemacs's own
`treemacs--persist' which runs at default depth 0)."
  (when declarative-project-treemacs-mode
    (declarative-project-treemacs--ensure-desired-state)
    (setq treemacs--workspaces declarative-project-treemacs--desired-state)
    (declarative-project-treemacs--save-cache)))

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
        ;; Layer 1: Pre-empt treemacs restore.  Setting this flag before
        ;; reading our cache prevents treemacs--maybe-load-workspaces from
        ;; ever firing when our mode is active.
        (put 'treemacs :state-is-restored t)
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
        ;; Layer 2: Safety-net advice in case treemacs--restore fires anyway.
        (advice-add 'treemacs--restore :after
                    #'declarative-project-treemacs--after-treemacs-restore)
        ;; Layer 3: Sync desired state into treemacs before its persist hook
        ;; writes to disk on shutdown.  Depth -90 ensures we run before
        ;; treemacs--persist (depth 0).
        (add-hook 'kill-emacs-hook
                  #'declarative-project-treemacs--sync-before-persist -90)
        (declarative-project-treemacs--read-cache)
        (when declarative-project-treemacs-autoprune
          (declarative-project-treemacs--prune-invalid-projects))
        (declarative-project-treemacs--override-workspaces)
        (add-hook 'declarative-project--apply-treemacs-workspaces-hook
                  #'declarative-project-treemacs--assign-declared-project)
        (add-hook 'treemacs-switch-workspace-hook
                  #'declarative-project-treemacs--on-workspace-switch)
        (add-hook 'treemacs-select-functions
                  #'declarative-project-treemacs--on-select))
    (declarative-project-treemacs--save-cache)
    ;; Clear pre-emption flag so treemacs resumes normal behavior.
    (put 'treemacs :state-is-restored nil)
    (advice-remove 'treemacs-add-and-display-current-project-exclusively
                   #'declarative-project-treemacs--around-exclusive-display)
    (when (fboundp 'treemacs-persp--ensure-workspace-exists)
      (advice-remove 'treemacs-persp--ensure-workspace-exists
                     #'declarative-project-treemacs--around-persp-ensure))
    (advice-remove 'treemacs--restore
                   #'declarative-project-treemacs--after-treemacs-restore)
    (remove-hook 'kill-emacs-hook
                 #'declarative-project-treemacs--sync-before-persist)
    (remove-hook 'declarative-project--apply-treemacs-workspaces-hook
                 #'declarative-project-treemacs--assign-declared-project)
    (remove-hook 'treemacs-switch-workspace-hook
                 #'declarative-project-treemacs--on-workspace-switch)
    (remove-hook 'treemacs-select-functions
                 #'declarative-project-treemacs--on-select)))

(defalias 'treemacs-declarative-workspaces-mode #'declarative-project-treemacs-mode)

(provide 'declarative-project-treemacs)
;;; declarative-project-treemacs.el ends here
