;;; declarative-project-mode.el --- Declarative Project mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: January 13, 2023
;; Modified: February 09, 2023
;; Version: 0.0.5
;; Keywords: project management, dependency management, declarative syntax, emacs minor-mode.
;; Homepage: https://github.com/cuttlefisch/declarative-project-mode
;; Package-Requires: ((emacs "25.1") (ghub "3.5.1") (treemacs "2.10") (yaml-mode "0.0.15") (yaml "0.5.1"))
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
  (project-file         ""      :type string)
  (required-resources   '()     :type list)
  (deps                 '()     :type list)
  (local-files          '()     :type list)
  (symlinks             '()     :type list)
  (agenda-files         '()     :type list)
  (workspaces           '()     :type list))

(defun declarative-project--make-declarative-project-with-defaults (&rest project-attrs)
  "Create a declarative-project with PROJECT-ATTRS."
  (apply 'make-declarative-project project-attrs))

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
  "When t don't prompt when removing project file paths from cache.."
  :type 'boolean
  :group 'declarative-project-mode)

(defvar declarative-project--cache-file
  (concat user-emacs-directory "declarative-project-cache.el")
  "Store path to all declared projects.")

(defvar declarative-project--cached-projects
  '()
  "List of declared projects' project specification file paths.")

(defvar declarative-project--github-url-regex-groups
  "\\(https://\\|git@\\)[a-zA-Z0-9_-]*\\.[a-zA-Z0-9_-]+[:\\/]\\([a-zA-Z0-9_-]+\\/\\)*\\([a-zA-Z0-9_-]+\\)\\(.git\\)?"
  "Regular expression to extract github username/repository-name from a github url.")

(defvar declarative-project-font-lock-keywords
  `(,@yaml-font-lock-keywords)
  "Match yaml font-lock keywords.")

(defun org-babel-execute:declarative-project (body params)
  "Execute command with Body and PARAMS from src block."
  ;; Prioritize targets of yaml block
  (let ((project-file (or (cdr (assoc :file params))
                          (cdr (assoc :tangle params))))
        (block-begin (org-element-property :begin (org-element-context)))
        (block-end (org-element-property :end (org-element-context))))
    ;; default value for src block params are "no" for some fields
    (pp (org-element-context))
    (pp (org-element-property :begin (org-element-context)))
    ;; TODO figure out how to represent location of src block
    ;;  including file name, begining, end of block
    ;;  OR use org-link format or similar
    (pp (org-store-link nil))
    (if (string= "no" project-file)
        (setq project-file (buffer-file-name)))
    (with-temp-file (org-babel-temp-file "project-")
      (insert body)
      (let ((project (declarative-project--read-project-from-buffer)))
        (setf (declarative-project-project-file project) project-file)
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
      ;; Capture groups:
      ;; 0          1               2             3                                   4
      ;; git@       github.com:     cuttlefisch/  treemacs-declarative-project-mode   .git
      ;; https://   github.com/     cuttlefisch/  prototype-emacs-devcontainer        .git
      (let ((repo-name (match-string 3 repo-url)))
        (warn "got repo data:\t%s" repo-name)
        ;; TODO this relies on :noerror flag from ghub
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

(defun declarative-project--apply-treemacs-workspaces (project)
  "Add project to any treemacs workspaces listed in PROJECT."
  (when-let ((workspaces (declarative-project-workspaces project)))
    (seq-map (lambda (workspace)
               (unless (treemacs-declarative-workspaces--workspace-memberp
                        (declarative-project-name project) workspace)
                 (treemacs-do-add-project-to-workspace
                  (declarative-project-root-directory project) workspace)))
    workspaces)
  (run-hook-with-args 'declarative-project--apply-treemacs-workspaces-hook project)))

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
  (seq-map (lambda (project-file)
             (when (file-exists-p project-file)
               (let ((project (declarative-project--read-project-from-file project-file)))
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
  "Filter missing projects from cached file paths."
  (let ((prev-cache declarative-project--cached-projects))
    (setq declarative-project--cached-projects
          (cl-remove-if-not #'file-exists-p declarative-project--cached-projects))
    (declarative-project--save-cache)
    (cl-set-difference prev-cache declarative-project--cached-projects)))

(defun declarative-project--save-cache ()
  "Save the list of cached projects to the cache file."
  (with-temp-file declarative-project--cache-file
    (mapc (lambda (str)
            (insert str)
            (newline))
          declarative-project--cached-projects)))

(defun declarative-project--append-to-cache (project-file)
  "Append PROJECT-FILE to declared project cache file."
  (append-to-file (format "%s\n" project-file) nil declarative-project--cache-file)
  (add-to-list 'declarative-project--cached-projects project-file)
  (declarative-project--save-cache))

(defun declarative-project--read-project-from-buffer (&optional buffername)
  "Return declarative-project defined by current buffer or BUFFERNAME."
  (with-current-buffer (or buffername (current-buffer))
    (let ((project-resources (yaml-parse-string (buffer-string)
                                                :null-object nil
                                                :sequence-type 'list)))
      (make-declarative-project
       :name (gethash 'name project-resources)
       :root-directory (or (gethash 'root-directory project-resources)
                           (gethash 'project-file project-resources))
       :required-resources (gethash 'required-resources project-resources)
       :deps (gethash 'deps project-resources)
       :local-files (gethash 'local-files project-resources)
       :symlinks (gethash 'symlinks project-resources)
       :agenda-files (gethash 'agenda-files project-resources)
       :workspaces (gethash 'workspaces project-resources)))))

(defun declarative-project--read-project-from-file (project-file)
  "Return the declarative-project defined in PROJECT-FILE."
  ;; TODO finish supporting org babel src block-defined projects here
  ;; Current behavior ::
  ;;    loads up PROJECT-FILE contents, assumed to be yaml
  ;;    sends off to read-project-from-buffer to parse yaml
  ;;
  ;; Desired behavior ::
  ;;    If file is an org file with begin/end position, or a link
  ;;    open src block at location and insert its contents into
  ;;    temp buffer. Then read project from that buffer
  (when (file-exists-p project-file)
    (with-temp-buffer
      (insert-file-contents project-file)
      (let ((project (declarative-project--read-project-from-buffer)))
        (setf (declarative-project-project-file project) project-file)))))

(defun declarative-project--install-project (&optional project project-file)
  "Step step through project spec & apply any blocks found."
  (interactive)
  (let* ((project-file (or project-file (expand-file-name "PROJECT.yaml" default-directory)))
         (project (or project
                      (declarative-project--read-project-from-file project-file)
                      (declarative-project--read-project-from-file (expand-file-name (buffer-file-name (current-buffer)))))))
    (declarative-project--prep-target project)
    (declarative-project--install-project-dependencies project)
    (declarative-project--copy-local-files project)
    (declarative-project--create-symlinks project)
    (declarative-project--apply-treemacs-workspaces project)
    (declarative-project--append-to-cache (declarative-project-project-file project))
    (declarative-project--check-required-resources project)
    (setq declarative-project--cached-projects (declarative-project--read-cache))
    (declarative-project--rebuild-org-agenda)
    (run-hook-with-args 'declarative-project--apply-treemacs-workspaces-hook project)
    (message "...Finished Installation!")))

(defun declarative-project--mode-setup ()
  "Load in cache, prune and handle agenda files."
  (when declarative-project-mode
    (message "Declarative Project Mode Enabled!")
    (yaml-mode)
    (setq declarative-project--cached-projects (declarative-project--read-cache))
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
