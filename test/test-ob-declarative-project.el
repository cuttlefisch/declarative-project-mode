;;; test-ob-declarative-project.el --- Tests for ob-declarative-project -*- lexical-binding: t -*-

;;; Commentary:
;; Buttercup test suite for org-babel integration.

;;; Code:

(require 'test-helper)
(require 'ob-declarative-project)

;;; ==========================================================================
;;; org-babel-execute:declarative-project
;;; ==========================================================================

(describe "org-babel-execute:declarative-project"
  (it "calls --install-from-content with body and dir"
    (let ((captured-content nil)
          (captured-dir nil))
      (spy-on 'declarative-project--install-from-content
              :and-call-fake
              (lambda (content dir &optional _extra)
                (setq captured-content content
                      captured-dir dir)
                (make-hash-table)))
      (org-babel-execute:declarative-project
       "project-name: test" '((:dir . "/tmp/myproject")))
      (expect captured-content :to-equal "project-name: test")
      (expect captured-dir :to-equal "/tmp/myproject")))

  (it "uses default-directory when :dir is absent"
    (let ((captured-dir nil))
      (spy-on 'declarative-project--install-from-content
              :and-call-fake
              (lambda (_content dir &optional _extra)
                (setq captured-dir dir)
                (make-hash-table)))
      (let ((default-directory "/tmp/fallback/"))
        (org-babel-execute:declarative-project
         "project-name: test" '()))
      (expect captured-dir :to-equal "/tmp/fallback/")))

  (it "returns a completion message"
    (spy-on 'declarative-project--install-from-content
            :and-return-value (make-hash-table))
    (expect (org-babel-execute:declarative-project
             "project-name: test" '((:dir . "/tmp")))
            :to-equal "Installation complete.")))

(provide 'test-ob-declarative-project)
;;; test-ob-declarative-project.el ends here
