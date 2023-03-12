;;; declarative-project-mode.el --- Declarative Project mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: January 13, 2023
;; Modified: March 09, 2023
;; Version: 0.0.6
;; Keywords: project management, dependency management, declarative syntax, emacs minor-mode.
;; Homepage: https://github.com/cuttlefisch/declarative-project-mode
;; Package-Requires: ((emacs "27.1") (ghub "3.5.1") (treemacs "2.10") (yaml-mode "0.0.15") (yaml "0.5.1"))
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
;; Declarative Project mode is a minor mode for managing project resources. The
;; mode is triggered by visiting a directory containing a PROJECT.yaml file. The
;; PROJECT.yaml file should be in yaml format and may contain the following
;; fields "name", "required-resources", "agenda-files", "deps", "local-files",
;; "symlinks", "treemacs-workspaces".
;;
;; Projects are cached in user-emacs-directory. The cache updates org agenda
;; with any declared agenda files from the cache upon startup. Users can
;; automatically prune missing projects from the cache, and create missing
;; agenda files by enabling declarative-project--auto-prune-cache and
;; declarative-project--persist-agenda-files respectively.
;;
;;; Code:
(require 'ghub)
(require 'vc)
(require 'treemacs)
(require 'yaml)
(require 'yaml-mode)
(require 'ob)
(require 'ob-eval)

(cl-defstruct declarative-project
  (name                 ""      :type string)
  (root-directory       ""      :type string)
  (source-file          ""      :type string)
  (required-resources   '()     :type list)
  (deps                 '()     :type list)
  (local-files          '()     :type list)
  (symlinks             '()     :type list)
  (agenda-files         '()     :type list)
  (workspaces           '()     :type list))

(defun declarative-project--make-declarative-project-with-defaults (&rest project-attrs)
  "Create a declarative-project with PROJECT-ATTRS."
  (apply 'make-declarative-project project-attrs))

;;;###autoload
(defvar declarative-project-mode nil
  "Var for declarative-workspaces-mode.")

(defcustom declarative-project--apply-treemacs-workspaces-hook nil
  "Hooks to run whenever the treemacs-workspaces are applied."
  :type 'hook
  :group 'declarative-project-mode-hooks)

(defcustom declarative-project--clobber nil
  "When t don't prompt for confirmation when overwriting local files."
  :type 'boolean
  :group 'declarative-project-mode)

(defcustom declarative-project--persist-agenda-files nil
  "Always recreate missing declared agenda files when t."
  :type 'boolean
  :group 'declarative-project-mode)

(defcustom declarative-project--auto-prune-cache nil
  "When t don't prompt when removing project file paths from cache."
  :type 'boolean
  :group 'declarative-project-mode)

(defvar declarative-project--cache-file
  (concat user-emacs-directory "declarative-project-cache.el")
  "Persistent cache of all declared projects.")

(defvar declarative-project--cached-projects
  '()
  "List of declared projects source file paths.")

(defvar declarative-project--github-url-regex-groups
  "\\(https://\\|git@\\)[a-zA-Z0-9_-]*\\.[a-zA-Z0-9_-]+[:\\/]\\([a-zA-Z0-9_-]+\\/\\)*\\([a-zA-Z0-9_-]+\\)\\(.git\\)?"
  "Regular expression to extract username/repository-name from a github url.

Capture groups:
---------------
  1         2           3            4                                    5
  git@     github.com:  cuttlefisch/ declarative-project-mode             .git
  git@     github.com:  cuttlefisch/ treemacs-declarative-project-mode    .git
  git@     github.com:  cuttlefisch/ prototype-emacs-devcontainer         .git
  git@     gitlab.com:  gitlab-org/  gitlab-docs                          .git
  https:// github.com/  cuttlefisch/ declarative-project-mode
  https:// github.com/  cuttlefisch/ treemacs-declarative-workspaces-mode
  https:// gitlab.com/  gitlab-org/  gitlab-docs")

(defvar declarative-project-source-file-link-regex-groups
  "\\(^\\/.*\\)::\\([0-9]+\\):\\([0-9]+\\)$"
  "Regular expression to match source file links matching filepath::begin:end.

`begin' and `end' are buffer positions for the beginning and end of an
org source block of type `declarative-project'.

Capture groups:
---------------
1                                                                    2      3
/home/username/RoamNotes/99999999999999-declared-projectfile.org ::  420 :  1085")

(defvar declarative-project-font-lock-keywords
  `(,@yaml-font-lock-keywords)
  "Match yaml font-lock keywords to fontify declarative-project src blocks.")

(defun declarative-project--source-linkp (link)
  "Return match if LINK matches declarative-project-source-file-link-regex."
  (let ((reb-re-syntax 'string))
    (when (string-match declarative-project-source-file-link-regex-groups link)
      `((:path . ,(match-string 1 link))
       (:begin . ,(string-to-number (match-string 2 link)))
       (:end . ,(string-to-number (match-string 3 link)))))))

(defun org-babel-execute:declarative-project (body params)
  "Execute command with BODY and PARAMS from src block."
  ;; Prioritize targets of yaml block over source org file
  (let ((source-file (cdr (assoc :tangle params)))
        (block-begin (org-element-property :begin (org-element-context)))
        (block-end (org-element-property :end (org-element-context))))

    ;; default value for src block params are "no" for some fields
    (if (string= "no" source-file)
        ;; Create link to source block begin & end `absolute/path.org::begin-char:end-char'
        (setq source-file (format "%s::%d:%s"
                                  (buffer-file-name) block-begin block-end)))
    ;; Install the project
    (with-temp-file (org-babel-temp-file "project-")
      (insert body)
      (let ((project (declarative-project--read-project-from-buffer)))
        ;; Make sure we have the correct source file
        (setf (declarative-project-source-file project) source-file)
        (declarative-project--install-project project)))))

(defun declarative-project--check-required-resources (project)
  "Warn if any resources labeled required in PROJECT are missing."
  (when-let ((required-resources (declarative-project-required-resources project)))
    (seq-map (lambda (resource)
               (unless (file-exists-p resource)
                 (warn (concat "Missing required resource: " resource))))
             required-resources)))

(defun declarative-project--repo-data (repository-full-name)
  "Return repository information from the github API for REPOSITORY-FULL-NAME."
  (let ((query (format "repos/%s" repository-full-name)))
    (ghub-get query nil :auth 'dpm :noerror t)))

(defun declarative-project--repo-data-from-url (repo-url)
  "Return best guess at project name from REPO-URL and return repo data."
  (let ((reb-re-syntax 'string))
    (when (string-match declarative-project--github-url-regex-groups repo-url)
      (let ((repo-name (match-string 3 repo-url)))
        ;; REVIEW this relies on :noerror flag from ghub
        (or (declarative-project--repo-data repo-name)
            `((name . ,repo-name)))))))

(defun declarative-project--install-project-dependencies (project)
  "Clone any git dependencies locally in PROJECT."
  (save-excursion
    (when-let ((project-deps (declarative-project-deps project)))
      (seq-map (lambda (dep)
                 (let* ((src (gethash 'src dep))
                        (repo-name (alist-get 'name
                                              (declarative-project--repo-data-from-url src)))
                        (dest (or (gethash 'dest dep) repo-name ))
                        (args (or (gethash 'args dep) ""))
                        (root-dir (declarative-project-root-directory project))
                        (dest-path (concat root-dir "/" dest)))
                   ;; Clone any git dependency unless destination already exists.
                   (if (and (file-exists-p dest-path) (not declarative-project--clobber))
                       (warn "Desintation already exists:\t%s" dest-path)
                     (progn
                       (warn "Cloning:\t%s" src)
                       (warn "To:\t%s" dest-path)
                       (vc-clone src 'Git (expand-file-name dest-path))))))
               project-deps))))

;; REVIEW is this still a problem?
;; TODO this fails if you try
;; local-files:
;;   - src: README.org
;;     dest: README.org
;; required-resources:
;;   - README.org
(defun declarative-project--copy-local-files (project)
  "Copy over any local files in PROJECT."
  (when-let ((local-files (declarative-project-local-files project)))
    (seq-map
     (lambda (file)
       (let* ((src (expand-file-name (gethash 'src file)))
              (root-dir (declarative-project-root-directory project))
              (dest (or (gethash 'dest file)
                        (file-name-nondirectory src)))
              (dest-path (concat root-dir "/" dest)))
         (message "root-dir\t")
         (cond
          ((file-directory-p src)
           (unless (file-directory-p dest-path)
             (copy-directory src
                             dest-path
                             t t t)))
          ((file-exists-p src)
           (copy-file src dest-path
                      t))
          (t
           (warn "No such file or directory:\t%s" src)))))
     local-files)))


(defun declarative-project--create-symlinks (project)
  "Create any symlinks in PROJECT struct."
  (when-let ((symlinks (declarative-project-symlinks project)))
    (seq-map (lambda (link-def)
               (let* ((targ (expand-file-name (gethash 'targ link-def)))
                      (link (or (gethash 'link link-def)
                                (file-name-nondirectory targ)))
                      (root-dir (declarative-project-root-directory project)))
                 (if (file-exists-p targ)
                     (progn
                       (make-directory (concat root-dir "/" (file-name-parent-directory link)) t)
                       (make-symbolic-link targ (concat root-dir "/" link) t))
                   (warn "No such file or directory:\t%s" targ))))
             symlinks)))


(defun declarative-project--prep-target (project)
  "Prepare install directory & update agenda files for PROJECT install."
  (let ((root-dir (declarative-project-root-directory project)))
    (if (not (file-exists-p root-dir))
        (if (or declarative-project--clobber
                (yes-or-no-p (format "Directory %s does not exist, create it? " root-dir)))
            (make-directory root-dir t)
          (warn "Installation Aborted")))
    (seq-map (lambda (agenda-file)
               (let* ((root-dir (declarative-project-root-directory project))
                      (file-path (concat root-dir "/" agenda-file)))
                 (unless (file-exists-p file-path)
                   (write-region "" nil file-path))
                 (unless (member file-path org-agenda-files)
                   (add-to-list 'org-agenda-files file-path))))
             (declarative-project-agenda-files project))
    (message "Finished adding agenda files")))


(defun declarative-project--rebuild-org-agenda ()
  "Add any valid agenda files from cached projects to org-agenda-files.
Any missing files will be created if declarative-project--persist-agenda-files."
  (seq-map (lambda (source-file)
             (when (file-exists-p source-file)
               (let ((project (declarative-project--read-project-from-file source-file)))
                 (when (declarative-project-p project)
                   (seq-map (lambda (agenda-file)
                              (let* ((root-dir (declarative-project-root-directory project))
                                     (file-path (concat root-dir "/" agenda-file)))
                                (unless (file-exists-p file-path)
                                  (warn "Missing declared agenda file:\t%s" file-path)
                                  (when declarative-project--persist-agenda-files
                                    (write-region "" nil file-path)))
                                (unless (member file-path org-agenda-files)
                                  (add-to-list 'org-agenda-files file-path))))
                            (declarative-project-agenda-files project))))))
           declarative-project--cached-projects))

(defun declarative-project--read-cache ()
  "Return list composed of newline separated files declaring projects."
  (save-excursion
    (with-temp-buffer
      (unless (file-exists-p declarative-project--cache-file)
        (declarative-project--save-cache))
      (insert-file-contents declarative-project--cache-file)
      (let ((raw-data (buffer-string)))
        (if (string-empty-p raw-data)
            '()
          (split-string raw-data "\n" t))))))

(defun declarative-project--prune-cache ()
  "Destrucively filter missing projects from cached file paths."
  (let ((prev-cache declarative-project--cached-projects))
    (setq declarative-project--cached-projects
          (cl-remove-if-not (lambda (source-file)
                              (or (file-exists-p source-file)
                                  (and-let* ((match-groups (declarative-project--source-linkp source-file))
                                             (path (alist-get :path match-groups))
                                             (file-exists-p path)))))
                            declarative-project--cached-projects))
    (declarative-project--save-cache)
    (cl-set-difference prev-cache declarative-project--cached-projects)))

(defun declarative-project--save-cache ()
  "Save the list of cached projects to the cache file."
  (with-temp-file declarative-project--cache-file
    (mapc (lambda (str)
            (insert str)
            (newline))
          declarative-project--cached-projects)))

(defun declarative-project--append-to-cache (source-file)
  "Append SOURCE-FILE to declared project cache file."
  (append-to-file (format "%s\n" source-file) nil declarative-project--cache-file)
  (add-to-list 'declarative-project--cached-projects source-file)
  (declarative-project--save-cache))

(defun declarative-project--read-project-from-buffer (&optional buffername)
  "Return declarative-project defined by current buffer or BUFFERNAME."
  (with-current-buffer (or buffername (current-buffer))
    (declarative-project--read-project-from-string (buffer-string))))

(defun declarative-project--read-project-from-string (&optional string)
  "Return declarative-project defined in yaml STRING."
  (let ((project-resources (yaml-parse-string (or string (buffer-string))
                                              :null-object nil
                                              :sequence-type 'list)))
    (make-declarative-project
     :name (gethash 'name project-resources)
     :root-directory (or (gethash 'root-directory project-resources)
                         (gethash 'source-file project-resources))
     :required-resources (gethash 'required-resources project-resources)
     :deps (gethash 'deps project-resources)
     :local-files (gethash 'local-files project-resources)
     :symlinks (gethash 'symlinks project-resources)
     :agenda-files (gethash 'agenda-files project-resources)
     :workspaces (gethash 'workspaces project-resources))))

(defun declarative-project--read-project-from-file (source-file)
  "Return the declarative-project defined at SOURCE-FILE."
  (save-excursion
    (with-temp-buffer
      (let ((project-string (if-let* ((match-groups (declarative-project--source-linkp source-file))
                                      (path (alist-get :path match-groups))
                                      (begin (alist-get :begin match-groups))
                                      (end (alist-get :end match-groups)))
                                (progn
                                  ;; Pull capture groups from matches
                                  ;; open buffer and insert src block contents from location
                                  ;; NOTE: parsing the buffer with the src block begin/end
                                  ;; present might still work b/c yaml parsing handles it fine,
                                  ;; but we explicitly use the src block value here.
                                  (insert-file-contents path nil (- begin 1) end)
                                  (org-element-property :value (org-element-at-point)))
                              (progn
                                (when (file-exists-p source-file)
                                  (insert-file-contents source-file)
                                  (buffer-string))))))
        (let ((project (declarative-project--read-project-from-string project-string)))
          (setf (declarative-project-source-file project) source-file)
          project)))))

(defun declarative-project--install-project-from-file (&optional source-file)
  "Install a declared project from SOURCE-FILE or `current-buffer-file'."
  (let ((source-file (or source-file
                         (buffer-file-name))))
    (declarative-project--install-project
     (declarative-project--read-project-from-file source-file))))

(defun declarative-project--install-project (&optional project source-file)
  "Step step through project spec & apply any blocks found."
  (interactive)
  (let* ((source-file (or source-file (expand-file-name "PROJECT.yaml" default-directory)))
         (project (or project
                      (declarative-project--read-project-from-file source-file)
                      (declarative-project--read-project-from-file (expand-file-name (buffer-file-name (current-buffer)))))))
    ;; Refresh cache just in case, seems to fix startup issue.
    (setq declarative-project--cached-projects (declarative-project--read-cache))
    (declarative-project--prep-target project)
    (declarative-project--install-project-dependencies project)
    (declarative-project--copy-local-files project)
    (declarative-project--create-symlinks project)
    (declarative-project--append-to-cache (declarative-project-source-file project))
    (setq declarative-project--cached-projects (declarative-project--read-cache))
    (declarative-project--check-required-resources project)
    (declarative-project--rebuild-org-agenda)
    (run-hook-with-args 'declarative-project--apply-treemacs-workspaces-hook project)
    (message "...Finished Installation!")))

(defun declarative-project--mode-setup ()
  "Load in cache, prune and handle agenda files."
  (when declarative-project-mode
    (message "Declarative Project Mode Enabled!")
    (setq declarative-project--cached-projects (declarative-project--read-cache))
    (warn "Found these projects boss:\n%s" declarative-project--cached-projects)
    (when declarative-project--auto-prune-cache
      (warn "WARNING :: Pruned the following projects from cache:\n%s"
            (mapconcat 'identity (declarative-project--prune-cache) "\n\t")))
    (declarative-project--rebuild-org-agenda)))

;;;###autoload
(define-derived-mode declarative-project-mode yaml-mode
  "Declarative Project mode."
  :init-value nil
  :lighter " DPM"
  :global t
  :group 'minor-modes
  (declarative-project--mode-setup))

(provide 'declarative-project-mode)
;;; declarative-project-mode.el ends here
