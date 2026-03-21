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
;;; cache validation
;;; ==========================================================================

(describe "cache validation"
  (it "every entry after save+read is a proper workspace/project struct"
    (with-treemacs-test-state
      (let ((proj (treemacs-project->create!
                   :name "P1" :path "/tmp/p1"
                   :path-status 'local-readable :is-disabled? nil)))
        (setq declarative-project-treemacs--desired-state
              (list (treemacs-workspace->create!
                     :name "WS1" :projects (list proj))))
        (declarative-project-treemacs--save-cache)
        (setq declarative-project-treemacs--desired-state nil)
        (declarative-project-treemacs--read-cache)
        (dolist (ws declarative-project-treemacs--desired-state)
          (expect (treemacs-workspace-p ws) :to-be-truthy)
          (expect (treemacs-workspace->name ws) :to-be-truthy)
          (dolist (proj (treemacs-workspace->projects ws))
            (expect (treemacs-project-p proj) :to-be-truthy)
            (expect (treemacs-project->name proj) :to-be-truthy)
            (expect (treemacs-project->path proj) :to-be-truthy))))))

  (it "no duplicate workspace names after repeated assign"
    (with-treemacs-test-state
      (declarative-project-treemacs--assign-project
       (list :name "P1" :path "/tmp/p1"
             :path-status 'local-readable :is-disabled? nil)
       "TestWS")
      (declarative-project-treemacs--assign-project
       (list :name "P2" :path "/tmp/p2"
             :path-status 'local-readable :is-disabled? nil)
       "TestWS")
      (declarative-project-treemacs--save-cache)
      (setq declarative-project-treemacs--desired-state nil)
      (declarative-project-treemacs--read-cache)
      (let ((names (mapcar #'treemacs-workspace->name
                           declarative-project-treemacs--desired-state)))
        (expect (length names) :to-equal (length (delete-dups (copy-sequence names)))))))

  (it "preserves exact project counts across workspaces"
    (with-treemacs-test-state
      (setq declarative-project-treemacs--desired-state
            (list (treemacs-workspace->create!
                   :name "Alpha"
                   :projects (list
                              (treemacs-project->create! :name "A1" :path "/tmp/a1"
                                                        :path-status 'local-readable :is-disabled? nil)
                              (treemacs-project->create! :name "A2" :path "/tmp/a2"
                                                        :path-status 'local-readable :is-disabled? nil)
                              (treemacs-project->create! :name "A3" :path "/tmp/a3"
                                                        :path-status 'local-readable :is-disabled? nil)))
                  (treemacs-workspace->create!
                   :name "Beta"
                   :projects (list
                              (treemacs-project->create! :name "B1" :path "/tmp/b1"
                                                        :path-status 'local-readable :is-disabled? nil)
                              (treemacs-project->create! :name "B2" :path "/tmp/b2"
                                                        :path-status 'local-readable :is-disabled? nil)))))
      (declarative-project-treemacs--save-cache)
      (setq declarative-project-treemacs--desired-state nil)
      (declarative-project-treemacs--read-cache)
      (let ((alpha (declarative-project-treemacs--workspaces-by-name "Alpha"))
            (beta (declarative-project-treemacs--workspaces-by-name "Beta")))
        (expect (length (treemacs-workspace->projects alpha)) :to-equal 3)
        (expect (length (treemacs-workspace->projects beta)) :to-equal 2))))

  (it "unassign persists correctly through save+read"
    (with-treemacs-test-state
      (let ((proj (treemacs-project->create!
                   :name "RemoveMe" :path "/tmp/rm"
                   :path-status 'local-readable :is-disabled? nil)))
        (setq declarative-project-treemacs--desired-state
              (list (treemacs-workspace->create!
                     :name "WS" :projects (list proj))))
        (declarative-project-treemacs--unassign-project "RemoveMe" "WS")
        ;; save+read cycle
        (setq declarative-project-treemacs--desired-state nil)
        (declarative-project-treemacs--read-cache)
        (let ((ws (declarative-project-treemacs--workspaces-by-name "WS")))
          (expect (treemacs-workspace->projects ws) :to-equal nil)))))

  (it "recovers gracefully from corrupted cache file"
    (with-treemacs-test-state
      (with-temp-file cache-file
        (insert "this is not valid elisp }{]["))
      (declarative-project-treemacs--read-cache)
      (expect declarative-project-treemacs--desired-state :to-be-truthy)
      (expect (length declarative-project-treemacs--desired-state) :to-equal 1)
      (expect (treemacs-workspace->name
               (car declarative-project-treemacs--desired-state))
              :to-equal "Default")))

  (it "recovers gracefully from empty cache file"
    (with-treemacs-test-state
      ;; Create a 0-byte file
      (with-temp-file cache-file
        (erase-buffer))
      (declarative-project-treemacs--read-cache)
      (expect declarative-project-treemacs--desired-state :to-be-truthy)
      (expect (length declarative-project-treemacs--desired-state) :to-equal 1)
      (expect (treemacs-workspace->name
               (car declarative-project-treemacs--desired-state))
              :to-equal "Default")))

  (it "save output contains expected setq form"
    (with-treemacs-test-state
      (setq declarative-project-treemacs--desired-state
            (list (treemacs-workspace->create! :name "Check" :projects nil)))
      (declarative-project-treemacs--save-cache)
      (let ((content (with-temp-buffer
                       (insert-file-contents cache-file)
                       (buffer-string))))
        (expect content :to-match "setq declarative-project-treemacs--desired-state"))))

  (it "double round-trip produces identical cache file content"
    (with-treemacs-test-state
      (setq declarative-project-treemacs--desired-state
            (list (treemacs-workspace->create!
                   :name "Stable"
                   :projects (list (treemacs-project->create!
                                    :name "SP" :path "/tmp/sp"
                                    :path-status 'local-readable :is-disabled? nil)))))
      (declarative-project-treemacs--save-cache)
      (setq declarative-project-treemacs--desired-state nil)
      (declarative-project-treemacs--read-cache)
      (declarative-project-treemacs--save-cache)
      (let ((first-content (with-temp-buffer
                             (insert-file-contents cache-file)
                             (buffer-string))))
        (setq declarative-project-treemacs--desired-state nil)
        (declarative-project-treemacs--read-cache)
        (declarative-project-treemacs--save-cache)
        (let ((second-content (with-temp-buffer
                                (insert-file-contents cache-file)
                                (buffer-string))))
          (expect first-content :to-equal second-content))))))

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
;;; declarative-project-treemacs--sync-workspace-list
;;; ==========================================================================

(describe "declarative-project-treemacs--sync-workspace-list"
  (it "sets treemacs--workspaces to desired state"
    (with-treemacs-test-state
      (let ((state (list (treemacs-workspace->create!
                           :name "SyncTest" :projects nil))))
        (setq declarative-project-treemacs--desired-state state)
        (declarative-project-treemacs--sync-workspace-list)
        (expect treemacs--workspaces :to-equal state))))

  (it "sets :state-is-restored flag"
    (with-treemacs-test-state
      (let ((state (list (treemacs-workspace->create!
                           :name "FlagTest" :projects nil))))
        (setq declarative-project-treemacs--desired-state state)
        (expect (get 'treemacs :state-is-restored) :not :to-be-truthy)
        (declarative-project-treemacs--sync-workspace-list)
        (expect (get 'treemacs :state-is-restored) :to-be t))))

  (it "does not change the current workspace in scope shelf"
    (with-treemacs-test-state
      (let* ((persp-ws (treemacs-workspace->create!
                         :name "Perspective main" :projects nil))
             (declared-ws (treemacs-workspace->create!
                            :name "MyProject" :projects nil)))
        (setf (treemacs-current-workspace) persp-ws)
        (setq declarative-project-treemacs--desired-state (list declared-ws))
        (declarative-project-treemacs--sync-workspace-list)
        ;; Current workspace should still be persp-ws, not overridden
        (expect (treemacs-current-workspace) :to-be persp-ws)))))

;;; ==========================================================================
;;; declarative-project-treemacs--on-select
;;; ==========================================================================

(describe "declarative-project-treemacs--on-select"
  (it "calls sync-workspace-list regardless of reason argument"
    (with-treemacs-test-state
      (spy-on 'declarative-project-treemacs--sync-workspace-list)
      (declarative-project-treemacs--on-select 'exists)
      (declarative-project-treemacs--on-select 'none)
      (expect 'declarative-project-treemacs--sync-workspace-list
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
              (expect (memq #'declarative-project-treemacs--on-workspace-switch
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
        (expect (memq #'declarative-project-treemacs--on-workspace-switch
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
  (it "calls sync-workspace-list when mode is active"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode t))
        (spy-on 'declarative-project-treemacs--sync-workspace-list)
        (let ((orig-called nil))
          (declarative-project-treemacs--around-persp-ensure
           (lambda () (setq orig-called t)))
          (expect 'declarative-project-treemacs--sync-workspace-list
                  :to-have-been-called)
          (expect orig-called :to-equal nil)))))

  (it "calls orig-fn when mode is not active"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode nil))
        (spy-on 'declarative-project-treemacs--sync-workspace-list)
        (let ((orig-called nil))
          (declarative-project-treemacs--around-persp-ensure
           (lambda () (setq orig-called t)))
          (expect 'declarative-project-treemacs--sync-workspace-list
                  :not :to-have-been-called)
          (expect orig-called :to-equal t))))))

;;; ==========================================================================
;;; Persistence defense (3-layer)
;;; ==========================================================================

(describe "persistence defense: pre-emption flag (Layer 1)"
  (it "sets :state-is-restored on enable"
    (with-treemacs-test-state
      (let ((declarative-project--apply-treemacs-workspaces-hook nil)
            (treemacs-switch-workspace-hook nil)
            (treemacs-select-functions nil))
        (put 'treemacs :state-is-restored nil)
        (declarative-project-treemacs-mode 1)
        (unwind-protect
            (expect (get 'treemacs :state-is-restored) :to-be t)
          (declarative-project-treemacs-mode -1)))))

  (it "clears :state-is-restored on disable"
    (with-treemacs-test-state
      (let ((declarative-project--apply-treemacs-workspaces-hook nil)
            (treemacs-switch-workspace-hook nil)
            (treemacs-select-functions nil))
        (declarative-project-treemacs-mode 1)
        (declarative-project-treemacs-mode -1)
        (expect (get 'treemacs :state-is-restored) :to-be nil)))))

(describe "persistence defense: after-restore advice (Layer 2)"
  (it "re-applies desired state when mode is active"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode t))
        (spy-on 'declarative-project-treemacs--override-workspaces)
        (declarative-project-treemacs--after-treemacs-restore)
        (expect 'declarative-project-treemacs--override-workspaces
                :to-have-been-called))))

  (it "does nothing when mode is inactive"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode nil))
        (spy-on 'declarative-project-treemacs--override-workspaces)
        (declarative-project-treemacs--after-treemacs-restore)
        (expect 'declarative-project-treemacs--override-workspaces
                :not :to-have-been-called))))

  (it "is added as advice on enable and removed on disable"
    (with-treemacs-test-state
      (let ((declarative-project--apply-treemacs-workspaces-hook nil)
            (treemacs-switch-workspace-hook nil)
            (treemacs-select-functions nil))
        (declarative-project-treemacs-mode 1)
        (unwind-protect
            (expect (advice-member-p
                     #'declarative-project-treemacs--after-treemacs-restore
                     'treemacs--restore)
                    :to-be-truthy)
          (declarative-project-treemacs-mode -1))
        (expect (advice-member-p
                 #'declarative-project-treemacs--after-treemacs-restore
                 'treemacs--restore)
                :to-equal nil)))))

(describe "persistence defense: kill-emacs sync (Layer 3)"
  (it "syncs desired state to treemacs--workspaces"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode t)
            (treemacs--workspaces nil))
        (setq declarative-project-treemacs--desired-state
              (list (treemacs-workspace->create! :name "Synced" :projects nil)))
        (declarative-project-treemacs--sync-before-persist)
        (expect (treemacs-workspace->name (car treemacs--workspaces))
                :to-equal "Synced"))))

  (it "does not overwrite current-workspace-name from treemacs scope"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode t)
            (treemacs--workspaces nil))
        ;; User switched to "hobby splines" earlier — name is already set
        (setq declarative-project-treemacs--current-workspace-name "hobby splines")
        (setq declarative-project-treemacs--desired-state
              (list (treemacs-workspace->create! :name "declarative project mode" :projects nil)
                    (treemacs-workspace->create! :name "hobby splines" :projects nil)))
        ;; Simulate daemon: treemacs-current-workspace returns wrong ws
        ;; (frame's scope shelf is gone, falls back to first/wrong workspace)
        (setf (treemacs-current-workspace)
              (car declarative-project-treemacs--desired-state))
        (declarative-project-treemacs--sync-before-persist)
        ;; The persisted name must still be "hobby splines", not overwritten
        (expect declarative-project-treemacs--current-workspace-name
                :to-equal "hobby splines"))))

  (it "adds kill-emacs-hook on enable and removes on disable"
    (with-treemacs-test-state
      (let ((declarative-project--apply-treemacs-workspaces-hook nil)
            (treemacs-switch-workspace-hook nil)
            (treemacs-select-functions nil))
        (declarative-project-treemacs-mode 1)
        (unwind-protect
            (expect (memq #'declarative-project-treemacs--sync-before-persist
                          kill-emacs-hook)
                    :to-be-truthy)
          (declarative-project-treemacs-mode -1))
        (expect (memq #'declarative-project-treemacs--sync-before-persist
                      kill-emacs-hook)
                :to-equal nil)))))

;;; ==========================================================================
;;; declarative-project-treemacs-reset-cache
;;; ==========================================================================

(describe "declarative-project-treemacs-reset-cache"
  (it "resets desired state to single Default workspace"
    (with-treemacs-test-state
      (setq declarative-project-treemacs--desired-state
            (list (treemacs-workspace->create! :name "Stale1" :projects nil)
                  (treemacs-workspace->create! :name "Stale2" :projects nil)))
      (declarative-project-treemacs-reset-cache)
      (expect (length declarative-project-treemacs--desired-state) :to-equal 1)
      (expect (treemacs-workspace->name
               (car declarative-project-treemacs--desired-state))
              :to-equal "Default")))

  (it "saves cache file after reset"
    (with-treemacs-test-state
      (setq declarative-project-treemacs--desired-state
            (list (treemacs-workspace->create! :name "Old" :projects nil)))
      (declarative-project-treemacs-reset-cache)
      ;; Re-read from disk and verify it was saved
      (setq declarative-project-treemacs--desired-state nil)
      (declarative-project-treemacs--read-cache)
      (expect (length declarative-project-treemacs--desired-state) :to-equal 1)
      (expect (treemacs-workspace->name
               (car declarative-project-treemacs--desired-state))
              :to-equal "Default")))

  (it "calls --override-workspaces when mode is active"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode t))
        (spy-on 'declarative-project-treemacs--override-workspaces)
        (declarative-project-treemacs-reset-cache)
        (expect 'declarative-project-treemacs--override-workspaces
                :to-have-been-called))))

  (it "does not call --override-workspaces when mode is inactive"
    (with-treemacs-test-state
      (let ((declarative-project-treemacs-mode nil))
        (spy-on 'declarative-project-treemacs--override-workspaces)
        (declarative-project-treemacs-reset-cache)
        (expect 'declarative-project-treemacs--override-workspaces
                :not :to-have-been-called)))))

;;; ==========================================================================
;;; current workspace persistence
;;; ==========================================================================

(describe "current workspace persistence"
  (it "save-cache persists current workspace name to file"
    (with-treemacs-test-state
      (let ((ws (treemacs-workspace->create! :name "Active" :projects nil)))
        (setq declarative-project-treemacs--desired-state (list ws))
        (setq declarative-project-treemacs--current-workspace-name "Active")
        (declarative-project-treemacs--save-cache)
        (let ((content (with-temp-buffer
                         (insert-file-contents cache-file)
                         (buffer-string))))
          (expect content :to-match "current-workspace-name")))))

  (it "round-trips current workspace name through save+read"
    (with-treemacs-test-state
      (let ((ws (treemacs-workspace->create! :name "Persisted" :projects nil)))
        (setq declarative-project-treemacs--desired-state (list ws))
        (setq declarative-project-treemacs--current-workspace-name "Persisted")
        (declarative-project-treemacs--save-cache)
        ;; Clear and reload
        (setq declarative-project-treemacs--current-workspace-name nil)
        (setq declarative-project-treemacs--desired-state nil)
        (declarative-project-treemacs--read-cache)
        (expect declarative-project-treemacs--current-workspace-name
                :to-equal "Persisted"))))

  (it "override-workspaces restores persisted name instead of first"
    (with-treemacs-test-state
      (let* ((ws-a (treemacs-workspace->create! :name "Alpha" :projects nil))
             (ws-b (treemacs-workspace->create! :name "Beta" :projects nil)))
        (setq declarative-project-treemacs--desired-state (list ws-a ws-b))
        (setq declarative-project-treemacs--current-workspace-name "Beta")
        ;; Current workspace is nil (simulating fresh restart)
        (setf (treemacs-current-workspace) nil)
        (declarative-project-treemacs--override-workspaces)
        (expect (treemacs-workspace->name (treemacs-current-workspace))
                :to-equal "Beta"))))

  (it "falls back to first when persisted name no longer exists"
    (with-treemacs-test-state
      (let ((ws (treemacs-workspace->create! :name "OnlyWS" :projects nil)))
        (setq declarative-project-treemacs--desired-state (list ws))
        (setq declarative-project-treemacs--current-workspace-name "Deleted")
        (setf (treemacs-current-workspace) nil)
        (declarative-project-treemacs--override-workspaces)
        (expect (treemacs-workspace->name (treemacs-current-workspace))
                :to-equal "OnlyWS"))))

  (it "on-workspace-switch records current workspace name"
    (with-treemacs-test-state
      (let ((ws (treemacs-workspace->create! :name "Switched" :projects nil)))
        (setq declarative-project-treemacs--desired-state (list ws))
        (setf (treemacs-current-workspace) ws)
        (declarative-project-treemacs--on-workspace-switch)
        (expect declarative-project-treemacs--current-workspace-name
                :to-equal "Switched"))))

  (it "on-workspace-switch persists name to disk"
    (with-treemacs-test-state
      (let ((ws (treemacs-workspace->create! :name "Persisted-Switch" :projects nil)))
        (setq declarative-project-treemacs--desired-state (list ws))
        (setf (treemacs-current-workspace) ws)
        (declarative-project-treemacs--on-workspace-switch)
        ;; Clear in-memory name and reload from disk
        (setq declarative-project-treemacs--current-workspace-name nil)
        (setq declarative-project-treemacs--desired-state nil)
        (declarative-project-treemacs--read-cache)
        (expect declarative-project-treemacs--current-workspace-name
                :to-equal "Persisted-Switch"))))

  (it "reset-cache clears current workspace name"
    (with-treemacs-test-state
      (setq declarative-project-treemacs--current-workspace-name "Stale")
      (declarative-project-treemacs-reset-cache)
      (expect declarative-project-treemacs--current-workspace-name :to-be nil))))

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
