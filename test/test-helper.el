;;; test-helper.el --- Test helpers for declarative-project-mode -*- lexical-binding: t -*-
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
;; Fixtures, macros, and stubs for Buttercup tests.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the package under test
(require 'declarative-project-mode)

;;; --- Fixtures ---

(defvar test-yaml-fixture
  "project-name: \"Test Project\"
required-resources:
  - README.org
  - LICENSE
deps:
  - src: git@github.com:user/repo.git
    dest: repo
  - src: https://github.com/user/other.git
    args: \"--branch dev\"
local-files:
  - src: /tmp/test-src-file
    dest: test-dest-file
symlinks:
  - targ: /tmp/test-symlink-target
    link: test-link
treemacs-workspaces:
  - \"Test Workspace\"
"
  "Sample YAML .project content for testing.")

(defvar test-json-fixture
  "{
  \"project-name\": \"Test Project\",
  \"required-resources\": [\"README.org\", \"LICENSE\"],
  \"deps\": [
    {\"src\": \"git@github.com:user/repo.git\", \"dest\": \"repo\"},
    {\"src\": \"https://github.com/user/other.git\", \"args\": \"--branch dev\"}
  ],
  \"local-files\": [
    {\"src\": \"/tmp/test-src-file\", \"dest\": \"test-dest-file\"}
  ],
  \"symlinks\": [
    {\"targ\": \"/tmp/test-symlink-target\", \"link\": \"test-link\"}
  ],
  \"treemacs-workspaces\": [\"Test Workspace\"]
}"
  "Sample JSON .project content for testing.")

(defvar test-invalid-content
  "this is neither yaml nor json {{{"
  "Content that is neither valid YAML nor valid JSON.")

;;; --- Macros ---

(defmacro with-temp-project-dir (&rest body)
  "Execute BODY in a temporary directory with a .project file context.
Binds `project-dir' to the temporary directory path."
  (declare (indent 0))
  `(let ((project-dir (make-temp-file "dpm-test-" t)))
     (unwind-protect
         (let ((default-directory (file-name-as-directory project-dir)))
           ,@body)
       (delete-directory project-dir t))))

(defmacro with-temp-project-file (content &rest body)
  "Create a temp project dir with .project file containing CONTENT, then run BODY.
Binds `project-dir' and `project-file'."
  (declare (indent 1))
  `(with-temp-project-dir
     (let ((project-file (expand-file-name ".project" project-dir)))
       (with-temp-file project-file
         (insert ,content))
       ,@body)))

;;; --- Treemacs stubs ---

;; Stub treemacs structs and functions so tests don't require treemacs.
;; Real treemacs uses cl-defstruct; we replicate just enough for the
;; treemacs extension module to work.

(unless (featurep 'treemacs)
  ;; --- Structs ---
  (cl-defstruct (treemacs-workspace (:constructor treemacs-workspace->create!))
    name projects is-disabled?)

  (cl-defstruct (treemacs-project (:constructor treemacs-project->create!))
    name path path-status is-disabled?)

  ;; --- Workspace API stubs ---
  (defvar treemacs--workspaces nil
    "Stub for treemacs workspace list.")

  (defvar treemacs-override-workspace nil
    "Stub for treemacs workspace override variable.")

  (defvar treemacs--current-workspace-stub nil
    "Stub storage for current workspace in tests.")

  (defun treemacs-current-workspace ()
    "Stub: return current workspace."
    treemacs--current-workspace-stub)

  (gv-define-setter treemacs-current-workspace (val)
    `(setq treemacs--current-workspace-stub ,val))

  (defun treemacs-do-create-workspace (&optional _name) '(success nil))
  (defun treemacs-find-workspace-by-name (_name) nil)
  (defun treemacs-do-add-project-to-workspace (_path _name) nil)
  (defvar treemacs-switch-workspace-hook nil
    "Stub for treemacs workspace switch hook.")

  (defvar treemacs-select-functions nil
    "Stub for treemacs select functions hook.")

  (defvar treemacs-project-follow-mode nil
    "Stub for treemacs-project-follow-mode.")

  (defvar-local treemacs--project-of-buffer nil
    "Stub for treemacs buffer-local project cache.")

  (defun treemacs-select-window (&optional _arg)
    "Stub for treemacs-select-window."
    nil)

  (defun treemacs-add-and-display-current-project-exclusively ()
    "Stub for treemacs-add-and-display-current-project-exclusively."
    nil))

;;; --- Treemacs test macro ---

(defmacro with-treemacs-test-state (&rest body)
  "Execute BODY with a clean treemacs desired-state and temporary cache file.
Binds `cache-file' to the temporary cache path."
  (declare (indent 0))
  `(let* ((cache-file (make-temp-file "dpm-treemacs-cache-" nil ".el"))
          (declarative-project-treemacs--desired-state nil)
          (declarative-project-treemacs--cache-file cache-file)
          (declarative-project-treemacs-mode nil)
          (treemacs--current-workspace-stub nil)
          (treemacs-select-functions nil)
          (treemacs-project-follow-mode nil))
     (unwind-protect
         (progn ,@body)
       (put 'treemacs :state-is-restored nil)
       (when (file-exists-p cache-file)
         (delete-file cache-file)))))

;;; --- Helper functions ---

(defun test-parse-yaml-to-hash (yaml-string)
  "Parse YAML-STRING to a hash table using the yaml library."
  (yaml-parse-string yaml-string :null-object nil :sequence-type 'list))

(defun test-parse-json-to-hash (json-string)
  "Parse JSON-STRING to a hash table."
  (json-parse-string json-string :null-object nil :array-type 'list))

(provide 'test-helper)
;;; test-helper.el ends here
