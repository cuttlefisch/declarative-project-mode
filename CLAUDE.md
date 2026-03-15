# declarative-project-mode

Emacs minor mode for declarative project resource management via `.project` files.

## Structure

- `declarative-project-mode.el` — core package (parsing, deps, files, symlinks, hooks)
- `declarative-project-treemacs.el` — optional treemacs workspace integration (absorbed from companion repo)
- `test/test-helper.el` — test fixtures, macros, treemacs stubs
- `test/test-declarative-project.el` — Buttercup test suite

## Dependencies

- `json` (built-in)
- `yaml` (external, required)
- `treemacs` (external, optional — soft dependency via `declare-function` and `featurep` guard)

## Key concepts

- `.project` files (YAML or JSON) declare: `project-name`, `required-resources`, `deps`, `local-files`, `symlinks`, `treemacs-workspaces`
- Mode activates automatically when visiting a `.project` file via `declarative-project--maybe-enable` on `find-file-hook`
- `C-c C-c i` runs installation
- `declarative-project--install-project` is the main entry point that processes all spec blocks
- `declarative-project--apply-treemacs-workspaces-hook` is run during install for treemacs integration
- Accessor functions: `declarative-project-workspaces`, `declarative-project-root-directory`, `declarative-project-name`

## Development

- Emacs Lisp with `lexical-binding: t`
- Package requires Emacs 28.1+
- Tests: `make test` (uses Buttercup, 25 specs)
- Test runner auto-detects straight.el (Doom) or Cask
