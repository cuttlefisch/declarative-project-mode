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

## Cache Architecture (treemacs integration)

- **Cache file**: `treemacs-declared-workspaces.el` in `user-emacs-directory` — stores desired workspace structs as a `setq` form
- **Desired-state model**: `.project` installs accumulate workspace/project structs into `--desired-state`; this list is the single source of truth for what treemacs workspaces should contain
- **Relationship to treemacs persist file**: treemacs has its own `treemacs-persist` file written on `kill-emacs-hook`. Our mode takes full control of `treemacs--workspaces`, so both files should converge.

### 3-layer persistence defense

- **Layer 1 (pre-empt)**: Sets `(put 'treemacs :state-is-restored t)` on mode enable, preventing `treemacs--maybe-load-workspaces` from ever firing
- **Layer 2 (after-restore advice)**: If `treemacs--restore` fires anyway (e.g. mode enabled late), `:after` advice re-applies desired state via `--override-workspaces`
- **Layer 3 (kill-emacs sync)**: `--sync-before-persist` runs on `kill-emacs-hook` at depth -90 (before treemacs's persist at depth 0), ensuring treemacs writes our desired state to disk

### treemacs-persp interaction

Doom's `:ui workspaces` module enables `treemacs-persp`, which sets scope type to `Perspectives` and creates "Perspective <name>" workspaces. We suppress this via `:around` advice on `treemacs-persp--ensure-workspace-exists`.

**Key design choice**: hooks that fire during normal operation (`treemacs-switch-workspace-hook`, `treemacs-select-functions`, persp-ensure advice) use `--sync-workspace-list` which maintains the workspace list without changing the current workspace. Only mode init and after-restore use `--override-workspaces` which also sets the current workspace. This prevents perspective switches and treemacs focus from resetting the user's workspace selection.

## Development

- Emacs Lisp with `lexical-binding: t`
- Package requires Emacs 28.1+
- Tests: `make test` (uses Buttercup, 96 specs)
- Test runner auto-detects straight.el (Doom) or Cask
