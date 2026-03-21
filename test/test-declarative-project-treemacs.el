;;; test-declarative-project-treemacs.el --- Tests for treemacs integration -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
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
;; Buttercup test suite for declarative-project-treemacs.

;;; Code:

(require 'test-helper)
(require 'declarative-project-treemacs)

;;; ==========================================================================
;;; declarative-project-treemacs--minimal-desired-state
;;; ==========================================================================

(describe "declarative-project-treemacs--minimal-desired-state"
  (it "returns a list with one Default workspace"
    (let ((state (declarative-project-treemacs--minimal-desired-state)))
      (expect (length state) :to-equal 1)
      (expect (treemacs-workspace->name (car state)) :to-equal "Default")))

  (it "creates a workspace with empty projects list"
    (let ((ws (car (declarative-project-treemacs--minimal-desired-state))))
      (expect (treemacs-workspace->projects ws) :to-equal nil))))

;;; ==========================================================================
;;; declarative-project-treemacs--ensure-desired-state
;;; ==========================================================================

(describe "declarative-project-treemacs--ensure-desired-state"
  (it "initializes state when nil"
    (with-treemacs-test-state
      (declarative-project-treemacs--ensure-desired-state)
      (expect declarative-project-treemacs--desired-state :to-be-truthy)
      (expect (length declarative-project-treemacs--desired-state) :to-equal 1)))

  (it "does not overwrite existing state"
    (with-treemacs-test-state
      (let ((custom-ws (treemacs-workspace->create! :name "Custom" :projects nil)))
        (setq declarative-project-treemacs--desired-state (list custom-ws))
        (declarative-project-treemacs--ensure-desired-state)
        (expect (treemacs-workspace->name
                 (car declarative-project-treemacs--desired-state))
                :to-equal "Custom")))))

;;; ==========================================================================
;;; declarative-project-treemacs--workspaces-by-name
;;; ==========================================================================

(describe "declarative-project-treemacs--workspaces-by-name"
  (it "finds a workspace by name"
    (with-treemacs-test-state
      (setq declarative-project-treemacs--desired-state
            (list (treemacs-workspace->create! :name "Alpha" :projects nil)
                  (treemacs-workspace->create! :name "Beta" :projects nil)))
      (let ((result (declarative-project-treemacs--workspaces-by-name "Beta")))
        (expect result :to-be-truthy)
        (expect (treemacs-workspace->name result) :to-equal "Beta"))))

  (it "returns nil when no workspace matches"
    (with-treemacs-test-state
      (setq declarative-project-treemacs--desired-state
            (list (treemacs-workspace->create! :name "Alpha" :projects nil)))
      (expect (declarative-project-treemacs--workspaces-by-name "Nope")
              :to-equal nil))))

;;; ==========================================================================
;;; declarative-project-treemacs--save-cache / --read-cache
;;; ==========================================================================

(describe "cache persistence"
  (it "round-trips desired state through save and read"
    (with-treemacs-test-state
      (let ((proj (treemacs-project->create!
                   :name "MyProj" :path "/tmp/myproj"
                   :path-status 'local-readable :is-disabled? nil)))
        (setq declarative-project-treemacs--desired-state
              (list (treemacs-workspace->create!
                     :name "TestWS" :projects (list proj))))
        (declarative-project-treemacs--save-cache)
        ;; Clear and reload
        (setq declarative-project-treemacs--desired-state nil)
        (declarative-project-treemacs--read-cache)
        (expect (length declarative-project-treemacs--desired-state) :to-equal 1)
        (let ((ws (car declarative-project-treemacs--desired-state)))
          (expect (treemacs-workspace->name ws) :to-equal "TestWS")
          (expect (length (treemacs-workspace->projects ws)) :to-equal 1)))))

  (it "initializes and saves when cache file does not exist"
    (with-treemacs-test-state
      (delete-file cache-file)
      (declarative-project-treemacs--read-cache)
      (expect declarative-project-treemacs--desired-state :to-be-truthy)
      (expect (file-exists-p cache-file) :to-be-truthy)))

  (it "normalizes old structs missing is-disabled? field after load"
    (with-treemacs-test-state
      ;; Write a cache file with old-layout structs (missing is-disabled?)
      (with-temp-file cache-file
        (insert ";; -*- no-byte-compile: t -*-\n")
        (insert "(setq declarative-project-treemacs--desired-state '(")
        ;; 2-field workspace: only name + projects, no is-disabled?
        (insert "#s(treemacs-workspace \"OldWS\" (")
        (insert "#s(treemacs-project \"OldProj\" \"/tmp/old\" local-readable nil)")
        (insert "))")
        (insert "))\n"))
      (declarative-project-treemacs--read-cache)
      (let ((ws (car declarative-project-treemacs--desired-state)))
        (expect (treemacs-workspace->name ws) :to-equal "OldWS")
        (expect (length (treemacs-workspace->projects ws)) :to-equal 1)
        ;; The key check: is-disabled? accessor must not error
        (expect (treemacs-workspace->is-disabled? ws) :not :to-throw)))))

;;; ==========================================================================
;;; declarative-project-treemacs--assign-project
;;; ==========================================================================

(describe "declarative-project-treemacs--assign-project"
  (it "creates a new workspace when target does not exist"
    (with-treemacs-test-state
      (declarative-project-treemacs--assign-project
       (list :name "Proj" :path "/tmp/proj"
             :path-status 'local-readable :is-disabled? nil)
       "NewWS")
      (let ((ws (declarative-project-treemacs--workspaces-by-name "NewWS")))
        (expect ws :to-be-truthy)
        (expect (length (treemacs-workspace->projects ws)) :to-equal 1))))

  (it "appends project to existing workspace"
    (with-treemacs-test-state
      (setq declarative-project-treemacs--desired-state
            (list (treemacs-workspace->create! :name "ExistingWS" :projects nil)))
      (declarative-project-treemacs--assign-project
       (list :name "Proj" :path "/tmp/proj"
             :path-status 'local-readable :is-disabled? nil)
       "ExistingWS")
      (let ((ws (declarative-project-treemacs--workspaces-by-name "ExistingWS")))
        (expect (length (treemacs-workspace->projects ws)) :to-equal 1))))

  (it "does not duplicate projects with the same name"
    (with-treemacs-test-state
      (setq declarative-project-treemacs--desired-state
            (list (treemacs-workspace->create! :name "WS" :projects nil)))
      (declarative-project-treemacs--assign-project
       (list :name "Proj" :path "/tmp/proj"
             :path-status 'local-readable :is-disabled? nil)
       "WS")
      (declarative-project-treemacs--assign-project
       (list :name "Proj" :path "/tmp/proj"
             :path-status 'local-readable :is-disabled? nil)
       "WS")
      (let ((ws (declarative-project-treemacs--workspaces-by-name "WS")))
        (expect (length (treemacs-workspace->projects ws)) :to-equal 1)))))

;;; ==========================================================================
;;; declarative-project-treemacs--unassign-project
;;; ==========================================================================

(describe "declarative-project-treemacs--unassign-project"
  (it "removes a project from a workspace"
    (with-treemacs-test-state
      (let ((proj (treemacs-project->create!
                   :name "Proj" :path "/tmp/proj"
                   :path-status 'local-readable :is-disabled? nil)))
        (setq declarative-project-treemacs--desired-state
              (list (treemacs-workspace->create!
                     :name "WS" :projects (list proj))))
        (declarative-project-treemacs--unassign-project "Proj" "WS")
        (let ((ws (declarative-project-treemacs--workspaces-by-name "WS")))
          (expect (treemacs-workspace->projects ws) :to-equal nil)))))

  (it "does not error when workspace does not exist (nil regression)"
    (with-treemacs-test-state
      (declarative-project-treemacs--ensure-desired-state)
      (expect (declarative-project-treemacs--unassign-project
               "Proj" "NonexistentWS")
              :not :to-throw))))

;;; ==========================================================================
;;; declarative-project-treemacs--remove-workspace
;;; ==========================================================================

(describe "declarative-project-treemacs--remove-workspace"
  (it "removes a workspace from desired state"
    (with-treemacs-test-state
      (setq declarative-project-treemacs--desired-state
            (list (treemacs-workspace->create! :name "Keep" :projects nil)
                  (treemacs-workspace->create! :name "Remove" :projects nil)))
      (declarative-project-treemacs--remove-workspace "Remove")
      (expect (length declarative-project-treemacs--desired-state) :to-equal 1)
      (expect (treemacs-workspace->name
               (car declarative-project-treemacs--desired-state))
              :to-equal "Keep"))))

;;; ==========================================================================
;;; declarative-project-treemacs--prune-invalid-projects
;;; ==========================================================================

(describe "declarative-project-treemacs--prune-invalid-projects"
  (it "removes projects whose paths do not exist"
    (with-treemacs-test-state
      (let ((valid-dir (make-temp-file "dpm-prune-" t)))
        (unwind-protect
            (progn
              (setq declarative-project-treemacs--desired-state
                    (list (treemacs-workspace->create!
                           :name "WS"
                           :projects
                           (list (treemacs-project->create!
                                  :name "Valid" :path valid-dir
                                  :path-status 'local-readable :is-disabled? nil)
                                 (treemacs-project->create!
                                  :name "Invalid" :path "/nonexistent/path"
                                  :path-status 'local-readable :is-disabled? nil)))))
              (declarative-project-treemacs--prune-invalid-projects)
              (let ((ws (car declarative-project-treemacs--desired-state)))
                (expect (length (treemacs-workspace->projects ws)) :to-equal 1)
                (expect (treemacs-project->name
                         (car (treemacs-workspace->projects ws)))
                        :to-equal "Valid")))
          (delete-directory valid-dir t))))))

;;; ==========================================================================
;;; declarative-project-treemacs--override-workspaces
;;; ==========================================================================

(describe "declarative-project-treemacs--override-workspaces"
  (it "sets treemacs--workspaces to desired state"
    (with-treemacs-test-state
      (let ((state (list (treemacs-workspace->create!
                          :name "Override" :projects nil))))
        (setq declarative-project-treemacs--desired-state state)
        (declarative-project-treemacs--override-workspaces)
        (expect treemacs--workspaces :to-equal state))))

  (it "sets :state-is-restored flag to prevent treemacs persist-file restore"
    (with-treemacs-test-state
      (let ((state (list (treemacs-workspace->create!
                          :name "FlagTest" :projects nil))))
        (setq declarative-project-treemacs--desired-state state)
        (expect (get 'treemacs :state-is-restored) :not :to-be-truthy)
        (declarative-project-treemacs--override-workspaces)
        (expect (get 'treemacs :state-is-restored) :to-be t))))

  (it "updates current workspace to matching object in desired state"
    (with-treemacs-test-state
      (let* ((old-ws (treemacs-workspace->create! :name "MyWS" :projects nil))
             (new-ws (treemacs-workspace->create! :name "MyWS" :projects nil)))
        ;; Simulate treemacs having a stale workspace in scope shelf
        (setf (treemacs-current-workspace) old-ws)
        (setq declarative-project-treemacs--desired-state (list new-ws))
        (declarative-project-treemacs--override-workspaces)
        ;; Current workspace should now be the new object, not the old one
        (expect (treemacs-current-workspace) :to-be new-ws))))

  (it "falls back to first desired workspace when current name is unknown"
    (with-treemacs-test-state
      (let* ((unknown-ws (treemacs-workspace->create! :name "Perspective main" :projects nil))
             (target-ws (treemacs-workspace->create! :name "MyProject" :projects nil)))
        (setf (treemacs-current-workspace) unknown-ws)
        (setq declarative-project-treemacs--desired-state (list target-ws))
        (declarative-project-treemacs--override-workspaces)
        (expect (treemacs-current-workspace) :to-be target-ws))))

  (it "falls back to first desired workspace when current workspace is nil"
    (with-treemacs-test-state
      (let ((target-ws (treemacs-workspace->create! :name "MyProject" :projects nil)))
        (setf (treemacs-current-workspace) nil)
        (setq declarative-project-treemacs--desired-state (list target-ws))
        (declarative-project-treemacs--override-workspaces)
        (expect (treemacs-current-workspace) :to-be target-ws))))

  (it "clears buffer project cache when switching workspace"
    (with-treemacs-test-state
      (let* ((old-ws (treemacs-workspace->create! :name "Old" :projects nil))
             (new-ws (treemacs-workspace->create! :name "New" :projects nil)))
        (setf (treemacs-current-workspace) old-ws)
        (setq declarative-project-treemacs--desired-state (list new-ws))
        ;; Set a buffer-local project cache value to verify it gets cleared
        (setq-local treemacs--project-of-buffer 'stale-project)
        (declarative-project-treemacs--override-workspaces)
        ;; treemacs--invalidate-buffer-project-cache is a define-inline that
        ;; clears treemacs--project-of-buffer in all buffers
        (expect treemacs--project-of-buffer :to-be nil)))))

;;; ==========================================================================
;;; declarative-project-treemacs--assign-declared-project (hook integration)
;;; ==========================================================================

(describe "declarative-project-treemacs--assign-declared-project"
  (it "assigns project to declared workspaces when mode is active"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode t)
            (resources (make-hash-table :test 'equal)))
        (puthash 'treemacs-workspaces '("DeclaredWS") resources)
        (puthash 'project-root "/tmp/declared/" resources)
        (puthash 'project-name "DeclaredProj" resources)
        (declarative-project-treemacs--assign-declared-project resources)
        (let ((ws (declarative-project-treemacs--workspaces-by-name "DeclaredWS")))
          (expect ws :to-be-truthy)
          (expect (treemacs-project->name
                   (car (treemacs-workspace->projects ws)))
                  :to-equal "DeclaredProj")))))

  (it "does nothing when mode is not active"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode nil)
            (resources (make-hash-table :test 'equal)))
        (puthash 'treemacs-workspaces '("WS") resources)
        (puthash 'project-root "/tmp/proj/" resources)
        (spy-on 'declarative-project-treemacs--assign-project)
        (declarative-project-treemacs--assign-declared-project resources)
        (expect 'declarative-project-treemacs--assign-project
                :not :to-have-been-called)))))

;;; ==========================================================================
;;; declarative-project-treemacs--on-select
;;; ==========================================================================

(describe "declarative-project-treemacs--on-select"
  (it "calls override-workspaces regardless of reason argument"
    (with-treemacs-test-state
      (spy-on 'declarative-project-treemacs--override-workspaces)
      (declarative-project-treemacs--on-select 'exists)
      (declarative-project-treemacs--on-select 'none)
      (expect 'declarative-project-treemacs--override-workspaces
              :to-have-been-called-times 2))))

;;; ==========================================================================
;;; declarative-project-treemacs-mode (hook add/remove)
;;; ==========================================================================

(describe "declarative-project-treemacs-mode"
  (it "adds hooks when enabled"
    (with-treemacs-test-state
      (let ((declarative-project--apply-treemacs-workspaces-hook nil)
            (treemacs-switch-workspace-hook nil)
            (treemacs-select-functions nil))
        (declarative-project-treemacs-mode 1)
        (unwind-protect
            (progn
              (expect (memq #'declarative-project-treemacs--assign-declared-project
                            declarative-project--apply-treemacs-workspaces-hook)
                      :to-be-truthy)
              (expect (memq #'declarative-project-treemacs--override-workspaces
                            treemacs-switch-workspace-hook)
                      :to-be-truthy)
              (expect (memq #'declarative-project-treemacs--on-select
                            treemacs-select-functions)
                      :to-be-truthy))
          (declarative-project-treemacs-mode -1)))))

  (it "removes hooks when disabled"
    (with-treemacs-test-state
      (let ((declarative-project--apply-treemacs-workspaces-hook nil)
            (treemacs-switch-workspace-hook nil)
            (treemacs-select-functions nil))
        (declarative-project-treemacs-mode 1)
        (declarative-project-treemacs-mode -1)
        (expect (memq #'declarative-project-treemacs--assign-declared-project
                      declarative-project--apply-treemacs-workspaces-hook)
                :to-equal nil)
        (expect (memq #'declarative-project-treemacs--override-workspaces
                      treemacs-switch-workspace-hook)
                :to-equal nil)
        (expect (memq #'declarative-project-treemacs--on-select
                      treemacs-select-functions)
                :to-equal nil))))

  (it "disables treemacs-project-follow-mode when enabled"
    (with-treemacs-test-state
      (let ((declarative-project--apply-treemacs-workspaces-hook nil)
            (treemacs-switch-workspace-hook nil)
            (treemacs-select-functions nil)
            (treemacs-project-follow-mode t))
        (spy-on 'treemacs-project-follow-mode)
        (declarative-project-treemacs-mode 1)
        (unwind-protect
            (expect 'treemacs-project-follow-mode
                    :to-have-been-called-with -1)
          (declarative-project-treemacs-mode -1))))))

;;; ==========================================================================
;;; declarative-project-treemacs--around-exclusive-display
;;; ==========================================================================

(describe "declarative-project-treemacs--around-exclusive-display"
  (it "calls treemacs-select-window when mode is active"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode t))
        (spy-on 'treemacs-select-window)
        (let ((orig-called nil))
          (declarative-project-treemacs--around-exclusive-display
           (lambda () (setq orig-called t)))
          (expect 'treemacs-select-window :to-have-been-called)
          (expect orig-called :to-equal nil)))))

  (it "calls orig-fn when mode is not active"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode nil))
        (spy-on 'treemacs-select-window)
        (let ((orig-called nil))
          (declarative-project-treemacs--around-exclusive-display
           (lambda () (setq orig-called t)))
          (expect 'treemacs-select-window :not :to-have-been-called)
          (expect orig-called :to-equal t))))))

;;; ==========================================================================
;;; declarative-project-treemacs--around-persp-ensure
;;; ==========================================================================

(describe "declarative-project-treemacs--around-persp-ensure"
  (it "calls override-workspaces when mode is active"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode t))
        (spy-on 'declarative-project-treemacs--override-workspaces)
        (let ((orig-called nil))
          (declarative-project-treemacs--around-persp-ensure
           (lambda () (setq orig-called t)))
          (expect 'declarative-project-treemacs--override-workspaces
                  :to-have-been-called)
          (expect orig-called :to-equal nil)))))

  (it "calls orig-fn when mode is not active"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode nil))
        (spy-on 'declarative-project-treemacs--override-workspaces)
        (let ((orig-called nil))
          (declarative-project-treemacs--around-persp-ensure
           (lambda () (setq orig-called t)))
          (expect 'declarative-project-treemacs--override-workspaces
                  :not :to-have-been-called)
          (expect orig-called :to-equal t))))))

;;; ==========================================================================
;;; Advice registration (mode enable/disable)
;;; ==========================================================================

(describe "declarative-project-treemacs-mode advice"
  (it "registers exclusive-display advice when enabled"
    (with-treemacs-test-state
      (let ((declarative-project--apply-treemacs-workspaces-hook nil)
            (treemacs-switch-workspace-hook nil)
            (treemacs-select-functions nil))
        (declarative-project-treemacs-mode 1)
        (unwind-protect
            (expect (advice-member-p
                     #'declarative-project-treemacs--around-exclusive-display
                     'treemacs-add-and-display-current-project-exclusively)
                    :to-be-truthy)
          (declarative-project-treemacs-mode -1)))))

  (it "removes exclusive-display advice when disabled"
    (with-treemacs-test-state
      (let ((declarative-project--apply-treemacs-workspaces-hook nil)
            (treemacs-switch-workspace-hook nil)
            (treemacs-select-functions nil))
        (declarative-project-treemacs-mode 1)
        (declarative-project-treemacs-mode -1)
        (expect (advice-member-p
                 #'declarative-project-treemacs--around-exclusive-display
                 'treemacs-add-and-display-current-project-exclusively)
                :to-equal nil)))))

(provide 'test-declarative-project-treemacs)
;;; test-declarative-project-treemacs.el ends here
