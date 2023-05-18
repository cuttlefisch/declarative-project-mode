;;; declarative-project-mode.el --- Declarative Project mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: January 13, 2023
;; Modified: May 11, 2023
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
;; (require 'ghub)
(require 'vc)
(require 'treemacs)
(require 'yaml)
(require 'yaml-mode)
(require 'ob)
(require 'ob-eval)
(require 'org)


;; ----------------------------------------------------------------------------
;; Project struct and initialization
;; ----------------------------------------------------------------------------
(cl-defstruct declarative-project
  (name                 ""      :type string)
  (root-directory       ""      :type string)
  (source-link          ""      :type string)
  (required-resources   '()     :type list)
  (deps                 '()     :type list)
  (local-files          '()     :type list)
  (symlinks             '()     :type list)
  (agenda-files         '()     :type list)
  (workspaces           '()     :type list))

(defun declarative-project--make-declarative-project-with-defaults (&rest project-attrs)
  "Create a declarative-project with PROJECT-ATTRS."
  (apply 'make-declarative-project project-attrs))


;; ----------------------------------------------------------------------------
;; Mode & Custom Variables
;; ----------------------------------------------------------------------------
;;;###autoload
(defvar declarative-project-mode nil
  "Var for declarative-workspaces-mode.")

;;;###autoload
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

(defvar declarative-project-source-link-link-regex-groups
  "\\(^\\/.*\\)::\\([0-9]+\\):\\([0-9]+\\)$"
  "Regular expression to match source file links matching filepath::begin:end.

`begin' and `end' are buffer positions for the beginning and end of an
org source block of type `declarative-project'.

Capture groups:
---------------
1                                                                    2      3
/home/username/RoamNotes/99999999999999-declared-projectfile.org ::  420 :  1085")

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


;; ----------------------------------------------------------------------------
;; Org Babel Integration
;; ----------------------------------------------------------------------------
(defvar declarative-project-font-lock-keywords
  `(,@yaml-font-lock-keywords)
  "Match yaml font-lock keywords to fontify declarative-project src blocks.")

(defun org-babel-execute:declarative-project (body params)
  "Execute command with BODY and PARAMS from src block."
  ;; Prioritize targets of yaml block over source org file
  (let ((tangle-target (cdr (assoc :tangle params)))
        (block-begin (org-element-property :begin (org-element-context)))
        (block-end (org-element-property :end (org-element-context))))

    ;; Default value for src block params are "no" for some fields --- Here we
    ;; only create a source-link to the src block if there's no target for
    ;; tangling. Otherwise, the source-link should point to the specified tangle
    ;; target.
    (let ((source-link (if (string= "no" tangle-target)
                           ;; Create link to source block begin & end `absolute/path.org::begin-char:end-char'
                           (format "%s::%d:%s"
                                   (buffer-file-name) block-begin block-end)
                         tangle-target)))
      ;; Install the project
      (with-temp-file (org-babel-temp-file "project-")
        (insert body)
        (let ((project (declarative-project--read-project-from-buffer)))
          ;; Make sure we have the correct source-link
          (setf (declarative-project-source-link project) source-link)
          (declarative-project--install-project project))))))

;; ----------------------------------------------------------------------------
;; Git Interactions
;; ----------------------------------------------------------------------------
;; (defun declarative-project--repo-data (repository-full-name)
;;   "Return repository information from the github API for REPOSITORY-FULL-NAME."
;;   (let ((query (format "repos/%s" repository-full-name)))
;;     (ghub-get query nil :auth 'dpm :noerror t)))

(defun declarative-project--repo-data-from-url (repo-url)
  "Return best guess at project name from REPO-URL and return repo data."
  (let ((reb-re-syntax 'string))
    (when (string-match declarative-project--github-url-regex-groups repo-url)
      (let ((repo-name (match-string 3 repo-url)))
        ;; REVIEW this relies on :noerror flag from ghub
        `((name . ,repo-name))))))


;; ----------------------------------------------------------------------------
;; Project Installation Helpers
;; ----------------------------------------------------------------------------
(defun declarative-project--check-required-resources (project)
  "Warn if any resources labeled required in PROJECT are missing."
  (when-let ((required-resources (declarative-project-required-resources project)))
    (seq-map (lambda (resource)
               (unless (file-exists-p resource)
                 (warn (concat "Missing required resource: " resource))))
             required-resources)))

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
         ;(message "root-dir\t")
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


;; ----------------------------------------------------------------------------
;; Handle Org Agenda
;; ----------------------------------------------------------------------------
(defun declarative-project--rebuild-org-agenda ()
  "Add any valid agenda files from cached projects to org-agenda-files.
Any missing files will be created if declarative-project--persist-agenda-files."
  (seq-map (lambda (source-link)
             (when (file-exists-p source-link)
               (let ((project (declarative-project--read-project-from-source-link source-link)))
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


;; ----------------------------------------------------------------------------
;; Handle Caching
;; ----------------------------------------------------------------------------
(defun declarative-project--source-linkp (link)
  "Return match if LINK matches declarative-project-source-link-link-regex."
  (let ((reb-re-syntax 'string))
    (when (string-match declarative-project-source-link-link-regex-groups link)
      `((:path . ,(match-string 1 link))
        (:begin . ,(string-to-number (match-string 2 link)))
        (:end . ,(string-to-number (match-string 3 link)))))))

(defun declarative-project--refresh-cache-from-file (&optional cache-file)
  "Update `declarative-project--cached-projects' from cache at CACHE-FILE."
  (let ((cache-file (or cache-file
                        declarative-project--cache-file)))
    (message "refreshing cache from cache-file:\t%s" cache-file)
    (setq declarative-project--cached-projects (declarative-project--read-cache cache-file))))

(defun declarative-project--read-cache (&optional cache-file)
  "Return list composed of newline separated files declaring projects at CACHE-FILE."
  (let ((cache-file (or cache-file
                        declarative-project--cache-file)))
    (save-excursion
      (with-temp-buffer
        ;; Initialize default cache file unless it already exists
        (unless (file-exists-p cache-file)
          (declarative-project--save-cache))
        (insert-file-contents cache-file)
        (let ((raw-data (buffer-string)))
          (if (string-empty-p raw-data)
              '()
            (split-string raw-data "\n" t)))))))

(defun declarative-project--save-cache (&optional cache-file)
  "Save the list of cached projects to the CACHE-FILE."
  (let ((cache-file (or cache-file
                        declarative-project--cache-file)))
  (with-temp-file cache-file
    (mapc (lambda (str)
            (insert str)
            (newline))
          declarative-project--cached-projects))))

(defun declarative-project--append-to-cache (source-link &optional cache-file)
  "Append SOURCE-LINK to CACHE-FILE containing declared project soure-links."
  (let ((cache-file (or cache-file
                        declarative-project--cache-file)))
    ;; BUG this append-to-file call creates duplicate cache entries if the :end of
    ;; the link is unique like so:
    ;; /home/heimdall/RoamNotes/20230113170058-declarative_project_minor_mode.org::425:1059
    ;; /home/heimdall/RoamNotes/20230113170058-declarative_project_minor_mode.org::425:1065
    (append-to-file (format "%s\n" source-link) nil cache-file)
    (add-to-list 'declarative-project--cached-projects source-link)
    (declarative-project--save-cache)))

;; BUG cl-remove-if-not lambda allows duplicate cache entries if the :end of the
;; link is unique like so:
;; /home/heimdall/RoamNotes/20230113170058-declarative_project_minor_mode.org::425:1059
;; /home/heimdall/RoamNotes/20230113170058-declarative_project_minor_mode.org::425:1065
;; Solution is to gather all
(defun declarative-project--prune-cache (&optional cache-file)
  "Destrucively filter missing and invalid projects from CACHE-FILE paths."
  ;; First remove all source-links pointing to invalid project source definitions
  (let ((cache-file (or cache-file
                        declarative-project--cache-file)))
  (declarative-project--refresh-cache-from-file cache-file)
  ;(message "working with these cached projects:\n%s" declarative-project--cached-projects)
  (setq declarative-project--cached-projects
        (cl-remove-if-not (lambda (source-link)
                            (message "checking project at:\t%s" source-link)
                            (or (let* ((match-groups (declarative-project--source-linkp source-link))
                                       (path (alist-get :path match-groups)))
                                  ;; (message "found match groups:\n%s" match-groups)
                                  ;; (message "for project path:\n%s" path)
                                  ;; To remain in the cache
                                  ;;   A) :path must be valid string
                                  ;;   B) File must exist at :path
                                  ;;   C) source block at SOURCE-LINK must be valid project
                                  (and (stringp path)
                                       (file-readable-p path)
                                       (declarative-project-p
                                        (declarative-project--read-project-from-source-link source-link))))
                                ;; In this case source-link /should/ represent a file-path
                                (file-exists-p source-link)))
                          declarative-project--cached-projects))
  ;; ;; BUG *NEXT* remove duplicate entries, where the :begin attrs match. Check
  ;; ;; the src at that point and find the correct :end, then update the cache.
  ;; ;;
  ;; ;; - For each unique combination of path & :begin
  ;; ;;          collect all entries in cache with that path
  ;; ;;          use temp buffer to find correct :end value
  ;; ;;          cache new entry with correct path::begin:end
  (declarative-project--save-cache cache-file)
  ;; (cl-set-difference prev-cache declarative-project--cached-projects)
  ))


;; ----------------------------------------------------------------------------
;; Utilities for parsing projects
;; ----------------------------------------------------------------------------
(defun declarative-project--project-string-from-source-link (source-link)
  "Return the :value of the org-element at SOURCE-LINK."
  (org-element-property :value (declarative-project--project-from-source-link source-link)))


(defun declarative-project--project-from-source-link (source-link)
  "Return yaml body of project given its SOURCE-LINK."
  (save-excursion
    (with-temp-buffer
      (if-let* ((match-groups (declarative-project--source-linkp source-link))
                (path (alist-get :path match-groups))
                (begin (alist-get :begin match-groups))
                (end (alist-get :end match-groups)))
          (progn
            ;; Pull capture groups from matches
            ;; open buffer and insert src block contents from location
            ;; NOTE: parsing the buffer with the src block begin/end
            ;; present might still work b/c yaml parsing handles it fine,
            ;; but we explicitly use the src block value here.
            ;; (message "Found source block for: %s" source-link)
            ;;(insert-file-contents path nil (- begin 1) end)
            ;; (message "Working with this yaml: \n%s" (buffer-string))
            (insert-file-contents path nil)
            (goto-char (+ begin 1))
            ;; (message "at char %s" begin)
            ;; (message "found element\n%s" (org-element-property :value (org-element-at-point)))
            )
        (when (file-exists-p source-link)
          (insert-file-contents source-link)
          (goto-char (point-min))))

      (condition-case err
          (progn
            ;(message "Found org element from source link:\n%s" (org-element-at-point))
            (org-element-at-point))
        (error (message "No valid org element at point.")
               "")))))




(defun declarative-project--read-project-from-string (&optional string)
  "Return declarative-project defined in yaml STRING."
  (let ((project-resources (condition-case err
                               (yaml-parse-string (or string (buffer-string))
                                                  :null-object nil
                                                  :sequence-type 'list)
                             (error (message "No valid yaml")
                                    nil))))
    (if project-resources
        (make-declarative-project
         :name (gethash 'name project-resources)
         :root-directory (or (gethash 'root-directory project-resources)
                             (gethash 'source-link project-resources))
         :required-resources (gethash 'required-resources project-resources)
         :deps (gethash 'deps project-resources)
         :local-files (gethash 'local-files project-resources)
         :symlinks (gethash 'symlinks project-resources)
         :agenda-files (gethash 'agenda-files project-resources)
         :workspaces (gethash 'workspaces project-resources))
      nil)))

(defun declarative-project--read-project-from-buffer (&optional buffername)
  "Return declarative-project defined by current buffer or BUFFERNAME."
  (with-current-buffer (or buffername (current-buffer))
    (declarative-project--read-project-from-string (buffer-string))))

(defun declarative-project--read-project-from-source-link (source-link)
  "Return the declarative-project defined at SOURCE-LINK."
  (let* ((project-string (declarative-project--project-string-from-source-link source-link))
         (project (or (declarative-project--read-project-from-string project-string) nil)))
    ;; (message "Checking project from source-link:\n%s" source-link)
    ;; (message "Checking project-string:\n%s" project-string)
    ;; (message "Valid project?\t%s" (declarative-project-p project))
    (if (declarative-project-p project)
        (progn
          (setf (declarative-project-source-link project) source-link)
          project)
      (progn
        (message "No valid project at %s" project-string)
        nil))))


;; ----------------------------------------------------------------------------
;; Mode Setup and core project installation behavior
;; ----------------------------------------------------------------------------
(defun declarative-project--install-project-from-file (&optional source-link)
  "Install a declared project from SOURCE-LINK or `current-buffer-file'."
  (let ((source-link (or source-link
                         (buffer-file-name))))
    (declarative-project--install-project
     (declarative-project--read-project-from-source-link source-link))))


(defun declarative-project--install-project (&optional project source-link)
  "Step step through project spec & apply any blocks found."
  (interactive)
  (let* ((source-link (or source-link (expand-file-name "PROJECT.yaml" default-directory)))
         (project (or project
                      (declarative-project--read-project-from-source-link source-link)
                      (declarative-project--read-project-from-source-link (expand-file-name (buffer-file-name (current-buffer)))))))
    ;; Refresh cache just in case, seems to fix startup issue.
    (declarative-project--refresh-cache-from-file)
    (declarative-project--prep-target project)
    (declarative-project--install-project-dependencies project)
    (declarative-project--copy-local-files project)
    (declarative-project--create-symlinks project)
    (declarative-project--append-to-cache (declarative-project-source-link project))
    (declarative-project--refresh-cache-from-file)
    (declarative-project--check-required-resources project)
    (declarative-project--rebuild-org-agenda)
    (run-hook-with-args 'declarative-project--apply-treemacs-workspaces-hook project)
    (message "...Finished Installation!")
    project))

;;;###autoload
(defun declarative-project--mode-setup ()
  "Load in cache, prune and handle agenda files."
  (message "Declarative Project Mode Enabled!")
  (declarative-project--refresh-cache-from-file)
                                        ;(message "Found these projects boss:\n%s" declarative-project--cached-projects)
  (when declarative-project--auto-prune-cache
    (declarative-project--prune-cache))
  (declarative-project--rebuild-org-agenda))

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
