(require 'ert)
(require 'declarative-project-mode)

(defun simple-hash-table (&rest pairs)
  (let ((le-hash (make-hash-table :test 'equal)))
    (dolist (pair pairs)
      (puthash (car pair) (cadr pair) le-hash))
    le-hash))

(ert-deftest declarative-project--install-project-dependencies-test ()
  (let ((project (declarative-project--make-declarative-project-with-defaults
                  :name "test"
                  :root-directory "/tmp/test"
                  :deps (list (simple-hash-table
                               '(src "git@github.com:cuttlefisch/declarative-project-mode.git")
                               '(dest "test-project")))))
        (declarative-project--clobber t)
        (install-target-path  "/tmp/test/test-project"))
    (declarative-project--prep-target project)
    (declarative-project--install-project-dependencies project)
    (when-let ((success (should (file-directory-p install-target-path))))
               (delete-directory (declarative-project-root-directory project) t)
      success)))

(ert-deftest declarative-project--copy-local-files-test ()
  (let ((project (declarative-project--make-declarative-project-with-defaults
                  :name "test"
                  :root-directory "/tmp/test"
                  :local-files (list (simple-hash-table
                                      '(src "./test/local-file.txt")
                                      '(dest "file.txt")))))
        (declarative-project--clobber t)
        (expected-resource  "/tmp/test/file.txt"))
    (declarative-project--prep-target project)
    (declarative-project--copy-local-files project)
    (when-let ((success (should (file-exists-p expected-resource))))
               (delete-directory (declarative-project-root-directory project) t)
      success)))

(ert-deftest declarative-project--create-symlinks-test ()
  (let ((project (declarative-project--make-declarative-project-with-defaults
                  :name "test"
                  :root-directory "/tmp/test"
                  :symlinks (list (simple-hash-table
                                   '(targ "./test/symlink-target.txt")
                                   '(link "test-link")))))
        (declarative-project--clobber t)
        (expected-resource  "/tmp/test/test-link"))
    (declarative-project--prep-target project)
    (declarative-project--create-symlinks project)
    (when-let ((success (should (file-symlink-p expected-resource))))
               (delete-directory (declarative-project-root-directory project) t)
      success)))


(ert-run-tests-batch-and-exit)
