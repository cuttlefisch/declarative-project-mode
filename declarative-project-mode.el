;;; declarative-project-mode.el --- Declarative Project mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: January 13, 2023
;; Modified: January 30, 2023
;; Version: 0.0.4
;; Keywords: project management, dependency management, declarative syntax, emacs minor-mode.
;; Homepage: https://github.com/cuttlefisch/declarative-project-mode
;; Package-Requires: ((emacs "26.1") (treemacs "2.10"))
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
;; Keybindings: - `C-c C-c i`: Run the install-project command when visiting
;; PROJECT.yaml file
;;
;;; Code:
(require 'yaml-mode)
(require 'yaml)
(require 'treemacs)

(cl-defstruct declarative-project
  (name                 ""      :type string)
  (root-directory       ""      :type string)
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

(defun declarative-project--check-required-resources (project)
  "Warn if any resources labeled required in PROJECT are missing."
  (when-let ((required-resources (declarative-project-required-resources project)))
    (seq-map (lambda (resource)
               (unless (file-exists-p resource)
                 (warn (concat "Missing required resource: " resource))))
             required-resources)))

(defun declarative-project--install-project-dependencies (project)
  "Clone any git dependencies locally in PROJECT."
  (when-let ((project-deps (declarative-project-deps project)))
    (seq-map (lambda (dep)
               (let* ((src (gethash 'src dep))
                      (dest (or (gethash 'dest dep) ""))
                      (args (or (gethash 'args dep) ""))
                      (root-dir (declarative-project-root-directory project))
                      (dest-path (concat root-dir "/" dest)))
                 ;; Clone any git dependency unless destination already
                 ;; exists.
                 (cond
                  ((and (file-exists-p dest-path) (not declarative-project--clobber))
                   (warn "Desintation already exists:\t%s" dest-path))
                  (t
                   (shell-command (concat "git clone " src
                                          " " (concat root-dir "/" dest)
                                          " " args))))))
             project-deps)))

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
               (treemacs-do-add-project-to-workspace
                (declarative-project-root-directory project)
                workspace))
             workspaces)
    (run-hooks 'declarative-project--apply-treemacs-workspaces-hook)))

(defun declarative-project--prep-target (project)
  "Prepare install directory & update agenda files for PROJECT install."
  (let ((root-dir (declarative-project-root-directory project)))
    (if (not (file-exists-p root-dir))
        (if (and  (file-writable-p root-dir)
                  (or declarative-project--clobber
                      (yes-or-no-p (format "Directory %s does not exist, create it? " root-dir))))
            (make-directory root-dir t)
          (message "Installation Aborted")))
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
             (let ((project (declarative-project--read-project-from-file project-file)))
               (seq-map (lambda (agenda-file)
                          (let* ((root-dir (declarative-project-root-directory project))
                                 (file-path (concat root-dir "/" agenda-file)))
                            (unless (file-exists-p file-path)
                              (warn "Missing declared agenda file:\t%s" file-path)
                              (when declarative-project--persist-agenda-files
                                (write-region "" nil file-path)))
                            (unless (member file-path org-agenda-files)
                              (add-to-list 'org-agenda-files file-path))))
                        (declarative-project-agenda-files project))))
           declarative-project--cached-projects))

(defun declarative-project--read-cache ()
  "Read project file paths from cache, and handle any change."
  (save-excursion
    (with-temp-buffer
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

(defun declarative-project--read-project-from-file (project-file)
  "Return the declarative-project defined in PROJECT-FILE."
  (when (file-exists-p project-file)
    (with-temp-buffer
      (insert-file-contents project-file)
      (let ((project-resources (yaml-parse-string (buffer-string)
                                                  :null-object nil
                                                  :sequence-type 'list)))
        (make-declarative-project
         :name (gethash 'project-name project-resources)
         :root-directory (or (gethash 'root-directory project-resources)
                             (gethash 'project-file project-resources))
         :required-resources (gethash 'required-resources project-resources)
         :deps (gethash 'deps project-resources)
         :local-files (gethash 'local-files project-resources)
         :symlinks (gethash 'symlinks project-resources)
         :agenda-files (gethash 'agenda-files project-resources)
         :workspaces (gethash 'workspaces project-resources))))))

(defun declarative-project--install-project ()
  "Step step through project spec & apply any blocks found."
  (interactive)
  (let* ((project-file (expand-file-name "PROJECT.yaml" default-directory))
         (project (declarative-project--read-project-from-file project-file)))
    (declarative-project--prep-target project)
    (declarative-project--check-required-resources project)
    (declarative-project--install-project-dependencies project)
    (declarative-project--copy-local-files project)
    (declarative-project--create-symlinks project)
    (declarative-project--apply-treemacs-workspaces project)
    (declarative-project--append-to-cache project-file)
    (message "...Finished Installation!")))

(defun declarative-project--mode-setup ()
  "Load in cache, prune and handle agenda files."
  (message "Declarative Project Mode Enabled!")
  (declarative-project--read-cache)
  (when declarative-project--auto-prune-cache
    (message "WARNING :: Pruned the following projects from cache:\n%s"
             (mapconcat 'identity (declarative-project--prune-cache) "\n\t")))
  (declarative-project--rebuild-org-agenda))

;;;###autoload
(define-globalized-minor-mode global-declarative-project-mode
  declarative-project-mode
  declarative-project--mode-setup
  :group 'declarative-project-mode)

;;;###autoload
(define-minor-mode declarative-project-mode
  "Declarative Project mode."
  :lighter " DPM"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c i")
                        'declarative-project--install-project)
            map)
  (if (or declarative-project-mode global-declarative-project-mode)
      (progn
        (message "Declarative Project Mode Enabled!")
        (setq declarative-project--cached-projects (declarative-project--read-cache))
        (when declarative-project--auto-prune-cache
          (message "WARNING :: Pruned the following projects from cache:\n%s"
                   (mapconcat 'identity (declarative-project--prune-cache) "\n\t")))
        (declarative-project--rebuild-org-agenda))))

(add-hook 'find-file-hook (lambda ()
                            (when (string-match-p "/PROJECT.yaml$" (buffer-file-name))
                              (declarative-project-mode 1))))
(provide 'declarative-project-mode)
;;; declarative-project-mode.el ends here
