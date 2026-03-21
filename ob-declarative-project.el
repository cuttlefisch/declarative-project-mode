;;; ob-declarative-project.el --- Org-Babel integration for declarative-project-mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <system.cuttle@gmail.com>
;; Maintainer: Hayden Stanko <system.cuttle@gmail.com>
;; Created: March 15, 2026
;; Modified: March 20, 2026
;; Version: 0.3.0
;; Keywords: convenience, tools, project
;; Homepage: https://github.com/cuttlefisch/declarative-project-mode
;; Package-Requires: ((emacs "28.1") (org "9.0") (declarative-project-mode "0.3.0"))
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
