;;; test-declarative-project.el --- Tests for declarative-project-mode -*- lexical-binding: t -*-

;;; Commentary:
;; Buttercup test suite covering all known bugs and core functionality.

;;; Code:

(require 'test-helper)

;;; ==========================================================================
;;; declarative-project--check-required-resources
;;; ==========================================================================

(describe "declarative-project--check-required-resources"
  (it "warns for each missing required resource"
    (with-temp-project-dir
      (let* ((resources (make-hash-table :test 'equal))
             (warnings '()))
        (puthash 'required-resources '("exists.txt" "missing.txt") resources)
        (write-region "" nil (expand-file-name "exists.txt"))
        (spy-on 'display-warning :and-call-fake
                (lambda (_type msg &rest _) (push msg warnings)))
        (declarative-project--check-required-resources resources)
        (expect (length warnings) :to-equal 1)
        (expect (car warnings) :to-match "missing.txt"))))

  (it "does nothing when all resources exist"
    (with-temp-project-dir
      (let ((resources (make-hash-table :test 'equal)))
        (puthash 'required-resources '("exists.txt") resources)
        (write-region "" nil (expand-file-name "exists.txt"))
        (spy-on 'display-warning)
        (declarative-project--check-required-resources resources)
        (expect 'display-warning :not :to-have-been-called))))

  (it "does nothing when required-resources key is absent"
    (let ((resources (make-hash-table :test 'equal)))
      (spy-on 'display-warning)
      (declarative-project--check-required-resources resources)
      (expect 'display-warning :not :to-have-been-called))))

;;; ==========================================================================
;;; declarative-project--install-project-dependencies
;;; ==========================================================================

(describe "declarative-project--install-project-dependencies"
  (it "calls git clone via call-process, not shell-command (shell injection safety)"
    (with-temp-project-dir
      (let ((resources (make-hash-table :test 'equal))
            (dep (make-hash-table :test 'equal)))
        (puthash 'src "https://github.com/user/repo.git" dep)
        (puthash 'dest "repo" dep)
        (puthash 'args "" dep)
        (puthash 'deps (list dep) resources)
        (spy-on 'call-process :and-return-value 0)
        (spy-on 'shell-command)
        (declarative-project--install-project-dependencies resources)
        (expect 'shell-command :not :to-have-been-called)
        (expect 'call-process :to-have-been-called))))

  (it "does not clone when dest directory already exists"
    (with-temp-project-dir
      (let ((resources (make-hash-table :test 'equal))
            (dep (make-hash-table :test 'equal)))
        (puthash 'src "https://github.com/user/repo.git" dep)
        (puthash 'dest "repo" dep)
        (puthash 'args "" dep)
        (puthash 'deps (list dep) resources)
        (make-directory (expand-file-name "repo"))
        (spy-on 'call-process)
        (declarative-project--install-project-dependencies resources)
        (expect 'call-process :not :to-have-been-called))))

  (it "infers dest from URL when dest is empty"
    (with-temp-project-dir
      (let ((resources (make-hash-table :test 'equal))
            (dep (make-hash-table :test 'equal)))
        (puthash 'src "https://github.com/user/my-repo.git" dep)
        (puthash 'deps (list dep) resources)
        (spy-on 'call-process :and-return-value 0)
        (declarative-project--install-project-dependencies resources)
        (expect 'call-process :to-have-been-called))))

  (it "passes args correctly to call-process"
    (with-temp-project-dir
      (let ((resources (make-hash-table :test 'equal))
            (dep (make-hash-table :test 'equal))
            (call-args nil))
        (puthash 'src "https://github.com/user/repo.git" dep)
        (puthash 'dest "repo" dep)
        (puthash 'args "--branch dev --depth 1" dep)
        (puthash 'deps (list dep) resources)
        (spy-on 'call-process :and-call-fake
                (lambda (&rest args) (setq call-args args) 0))
        (declarative-project--install-project-dependencies resources)
        (expect call-args :to-contain "--branch")
        (expect call-args :to-contain "dev")
        (expect call-args :to-contain "--depth")
        (expect call-args :to-contain "1")))))

;;; ==========================================================================
;;; declarative-project--copy-local-files
;;; ==========================================================================

(describe "declarative-project--copy-local-files"
  (it "copies a file to the project directory"
    (with-temp-project-dir
      (let ((resources (make-hash-table :test 'equal))
            (file-spec (make-hash-table :test 'equal))
            (src-file (make-temp-file "dpm-src-")))
        (unwind-protect
            (progn
              (with-temp-file src-file (insert "test content"))
              (puthash 'src src-file file-spec)
              (puthash 'dest "copied.txt" file-spec)
              (puthash 'local-files (list file-spec) resources)
              (declarative-project--copy-local-files resources)
              (expect (file-exists-p (expand-file-name "copied.txt")) :to-be-truthy))
          (delete-file src-file)))))

  (it "copies a directory to the project directory"
    (with-temp-project-dir
      (let ((resources (make-hash-table :test 'equal))
            (file-spec (make-hash-table :test 'equal))
            (src-dir (make-temp-file "dpm-srcdir-" t)))
        (unwind-protect
            (progn
              (write-region "x" nil (expand-file-name "file.txt" src-dir))
              (puthash 'src src-dir file-spec)
              (puthash 'dest "copied-dir" file-spec)
              (puthash 'local-files (list file-spec) resources)
              (declarative-project--copy-local-files resources)
              (expect (file-directory-p (expand-file-name "copied-dir")) :to-be-truthy))
          (delete-directory src-dir t)))))

  (it "warns when source does not exist"
    (with-temp-project-dir
      (let ((resources (make-hash-table :test 'equal))
            (file-spec (make-hash-table :test 'equal)))
        (puthash 'src "/nonexistent/path/file.txt" file-spec)
        (puthash 'dest "out.txt" file-spec)
        (puthash 'local-files (list file-spec) resources)
        (spy-on 'display-warning)
        (declarative-project--copy-local-files resources)
        (expect 'display-warning :to-have-been-called)))))

;;; ==========================================================================
;;; declarative-project--create-symlinks
;;; ==========================================================================

(describe "declarative-project--create-symlinks"
  (it "creates a symlink to the target"
    (with-temp-project-dir
      (let ((resources (make-hash-table :test 'equal))
            (link-spec (make-hash-table :test 'equal))
            (target-file (make-temp-file "dpm-targ-")))
        (unwind-protect
            (progn
              (puthash 'targ target-file link-spec)
              (puthash 'link "my-link" link-spec)
              (puthash 'symlinks (list link-spec) resources)
              (declarative-project--create-symlinks resources)
              (let ((link-path (expand-file-name "my-link")))
                (expect (file-symlink-p link-path) :to-be-truthy)))
          (delete-file target-file)))))

  (it "warns when target does not exist"
    (with-temp-project-dir
      (let ((resources (make-hash-table :test 'equal))
            (link-spec (make-hash-table :test 'equal)))
        (puthash 'targ "/nonexistent/target" link-spec)
        (puthash 'link "my-link" link-spec)
        (puthash 'symlinks (list link-spec) resources)
        (spy-on 'display-warning)
        (declarative-project--create-symlinks resources)
        (expect 'display-warning :to-have-been-called))))

  (it "creates parent directories for the link"
    (with-temp-project-dir
      (let ((resources (make-hash-table :test 'equal))
            (link-spec (make-hash-table :test 'equal))
            (target-file (make-temp-file "dpm-targ-")))
        (unwind-protect
            (progn
              (puthash 'targ target-file link-spec)
              (puthash 'link "subdir/nested/my-link" link-spec)
              (puthash 'symlinks (list link-spec) resources)
              (declarative-project--create-symlinks resources)
              (let ((link-path (expand-file-name "subdir/nested/my-link")))
                (expect (file-symlink-p link-path) :to-be-truthy)))
          (delete-file target-file))))))

;;; ==========================================================================
;;; declarative-project--apply-treemacs-workspaces
;;; ==========================================================================

(describe "declarative-project--apply-treemacs-workspaces"
  (it "runs the hook with project-resources"
    (with-temp-project-dir
      (let ((resources (make-hash-table :test 'equal))
            (hook-called nil))
        (puthash 'treemacs-workspaces '("WS1") resources)
        (puthash 'project-file (expand-file-name ".project") resources)
        (puthash 'project-name "Test" resources)
        (let ((declarative-project--apply-treemacs-workspaces-hook
               (list (lambda (res) (setq hook-called t)))))
          (declarative-project--apply-treemacs-workspaces resources)
          (expect hook-called :to-be-truthy)))))

  (it "does nothing when treemacs-workspaces key is absent"
    (let ((resources (make-hash-table :test 'equal)))
      (spy-on 'run-hook-with-args)
      (declarative-project--apply-treemacs-workspaces resources)
      (expect 'run-hook-with-args :not :to-have-been-called))))

;;; ==========================================================================
;;; declarative-project--install-from-content
;;; ==========================================================================

(describe "declarative-project--install-from-content"
  (it "installs from a YAML string without a .project file on disk"
    (with-temp-project-dir
      (spy-on 'declarative-project--check-required-resources)
      (spy-on 'declarative-project--install-project-dependencies)
      (spy-on 'declarative-project--copy-local-files)
      (spy-on 'declarative-project--create-symlinks)
      (spy-on 'declarative-project--apply-treemacs-workspaces)
      (let ((result (declarative-project--install-from-content
                     test-yaml-fixture project-dir)))
        (expect 'declarative-project--check-required-resources :to-have-been-called)
        (expect (hash-table-p result) :to-be-truthy)
        (expect (gethash 'project-root result) :to-equal
                (file-name-as-directory (expand-file-name project-dir))))))

  (it "installs from a JSON string without a .project file on disk"
    (with-temp-project-dir
      (spy-on 'declarative-project--check-required-resources)
      (spy-on 'declarative-project--install-project-dependencies)
      (spy-on 'declarative-project--copy-local-files)
      (spy-on 'declarative-project--create-symlinks)
      (spy-on 'declarative-project--apply-treemacs-workspaces)
      (let ((result (declarative-project--install-from-content
                     test-json-fixture project-dir)))
        (expect 'declarative-project--check-required-resources :to-have-been-called)
        (expect (hash-table-p result) :to-be-truthy))))

  (it "passes extra-keys into the resource hash"
    (with-temp-project-dir
      (spy-on 'declarative-project--check-required-resources)
      (spy-on 'declarative-project--install-project-dependencies)
      (spy-on 'declarative-project--copy-local-files)
      (spy-on 'declarative-project--create-symlinks)
      (spy-on 'declarative-project--apply-treemacs-workspaces)
      (let ((result (declarative-project--install-from-content
                     test-yaml-fixture project-dir
                     (list (cons 'my-key "my-value")))))
        (expect (gethash 'my-key result) :to-equal "my-value")))))

;;; ==========================================================================
;;; declarative-project--install-project (parsing)
;;; ==========================================================================

(describe "declarative-project--install-project"
  (it "parses valid YAML"
    (with-temp-project-file test-yaml-fixture
      (spy-on 'declarative-project--check-required-resources)
      (spy-on 'declarative-project--install-project-dependencies)
      (spy-on 'declarative-project--copy-local-files)
      (spy-on 'declarative-project--create-symlinks)
      (spy-on 'declarative-project--apply-treemacs-workspaces)
      (declarative-project--install-project)
      (expect 'declarative-project--check-required-resources :to-have-been-called)))

  (it "falls back to JSON when YAML fails"
    (with-temp-project-file test-json-fixture
      (spy-on 'declarative-project--check-required-resources)
      (spy-on 'declarative-project--install-project-dependencies)
      (spy-on 'declarative-project--copy-local-files)
      (spy-on 'declarative-project--create-symlinks)
      (spy-on 'declarative-project--apply-treemacs-workspaces)
      (declarative-project--install-project)
      (expect 'declarative-project--check-required-resources :to-have-been-called)))

  (it "signals user-error when both YAML and JSON fail"
    (with-temp-project-file test-invalid-content
      (expect (declarative-project--install-project) :to-throw 'user-error))))

;;; ==========================================================================
;;; declarative-project--maybe-enable (find-file-hook)
;;; ==========================================================================

(describe "declarative-project--maybe-enable"
  (it "activates mode on .project files"
    (with-temp-project-dir
      (let ((project-file (expand-file-name ".project")))
        (write-region "project-name: test" nil project-file)
        (with-current-buffer (find-file-noselect project-file)
          (unwind-protect
              (expect declarative-project-mode :to-be-truthy)
            (kill-buffer))))))

  (it "does not crash when buffer-file-name is nil"
    (with-temp-buffer
      (expect (declarative-project--maybe-enable) :not :to-throw))))

;;; ==========================================================================
;;; Accessor functions
;;; ==========================================================================

(describe "project accessor functions"
  (it "declarative-project-workspaces returns treemacs-workspaces value"
    (let ((resources (make-hash-table :test 'equal)))
      (puthash 'treemacs-workspaces '("WS1" "WS2") resources)
      (expect (declarative-project-workspaces resources) :to-equal '("WS1" "WS2"))))

  (it "declarative-project-root-directory prefers project-root over project-file"
    (let ((resources (make-hash-table :test 'equal)))
      (puthash 'project-file "/home/user/other/.project" resources)
      (puthash 'project-root "/home/user/myproject/" resources)
      (expect (declarative-project-root-directory resources)
              :to-equal "/home/user/myproject/")))

  (it "declarative-project-root-directory falls back to project-file directory"
    (let ((resources (make-hash-table :test 'equal)))
      (puthash 'project-file "/home/user/myproject/.project" resources)
      (expect (declarative-project-root-directory resources)
              :to-equal "/home/user/myproject/")))

  (it "declarative-project-name returns project-name value"
    (let ((resources (make-hash-table :test 'equal)))
      (puthash 'project-name "My Project" resources)
      (expect (declarative-project-name resources) :to-equal "My Project"))))

;;; ==========================================================================
;;; defgroup / defcustom
;;; ==========================================================================

(describe "customization"
  (it "defines the declarative-project customization group"
    (expect (get 'declarative-project 'custom-group) :to-be-truthy))

  (it "defines declarative-project-auto-install custom variable"
    (expect (custom-variable-p 'declarative-project-auto-install) :to-be-truthy)))

(provide 'test-declarative-project)
;;; test-declarative-project.el ends here
