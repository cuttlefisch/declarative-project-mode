;;; test-helper.el --- Test helpers for declarative-project-mode -*- lexical-binding: t -*-

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

  (defun treemacs-do-create-workspace (&optional _name) '(success nil))
  (defun treemacs-find-workspace-by-name (_name) nil)
  (defmacro treemacs-with-workspace (_ws &rest body)
    (declare (indent 1))
    `(progn ,@body))
  (defun treemacs-do-add-project-to-workspace (_path _name) nil)
  (defvar treemacs-switch-workspace-hook nil
    "Stub for treemacs workspace switch hook."))

;;; --- Treemacs test macro ---

(defmacro with-treemacs-test-state (&rest body)
  "Execute BODY with a clean treemacs desired-state and temporary cache file.
Binds `cache-file' to the temporary cache path."
  (declare (indent 0))
  `(let* ((cache-file (make-temp-file "dpm-treemacs-cache-" nil ".el"))
          (declarative-project-treemacs--desired-state nil)
          (declarative-project-treemacs--cache-file cache-file)
          (declarative-project-treemacs-mode nil))
     (unwind-protect
         (progn ,@body)
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
