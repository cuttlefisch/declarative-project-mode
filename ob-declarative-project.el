;;; ob-declarative-project.el --- Org-Babel integration for declarative-project-mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: March 15, 2026
;; Modified: March 20, 2026
;; Version: 0.3.0
;; Keywords: convenience, tools, project
;; Homepage: https://github.com/cuttlefisch/declarative-project-mode
;; Package-Requires: ((emacs "28.1") (org "9.0") (declarative-project-mode "0.3.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Org-Babel support for declarative-project src blocks.
;;
;; Add `(declarative-project . t)' to `org-babel-load-languages' to enable.
;; Then use `C-c C-c' on a src block like:
;;
;;   #+begin_src declarative-project :dir ~/my-project
;;   project-name: "My Project"
;;   deps:
;;     - src: git@github.com:user/repo.git
;;   #+end_src
;;
;;; Code:

(require 'ob)
(require 'declarative-project-mode)

;;;###autoload
(defun org-babel-execute:declarative-project (body params)
  "Execute a declarative-project source block.
BODY is YAML/JSON content.  The `:dir' header sets project root
\(defaults to `default-directory')."
  (let ((project-dir (or (cdr (assq :dir params)) default-directory)))
    (declarative-project--install-from-content body project-dir)
    "Installation complete."))

(provide 'ob-declarative-project)
;;; ob-declarative-project.el ends here
