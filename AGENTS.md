# AGENTS.md — Emacs Configuration Repository

## Overview

This is a personal Emacs configuration written in **literate org-mode** style. Configuration
source files live in `config-org/*.org` and are tangled (via `org-babel-tangle`) into
`config-elisp/*.el`, which Emacs loads at runtime.

**Primary language focus:** Elixir (deep LSP integration, test tooling, IEx completion)
**Primary platform:** macOS (Linux/NixOS secondary, guarded with `(when (eq system-type 'darwin) ...)`)

---

## Critical Workflow Rule

> **Always edit `config-org/*.org` files. Never edit `config-elisp/*.el` directly.**

The `.el` files in `config-elisp/` are generated artifacts. Edits made directly to them
will be overwritten the next time the corresponding `.org` file is tangled.

After editing an `.org` file, tangle it with `M-x org-babel-tangle` (or `C-c C-v t`) to
regenerate the corresponding `.el` file. Both the source and tangled output are tracked in git.

---

## Repository Structure

```
config-org/          # SOURCE: literate org-mode config files (edit these)
config-elisp/        # GENERATED: tangled .el output (do not edit directly)
elisp/               # Hand-written custom elisp (expreg.el, pretty-fonts.el)
snippets/            # YASnippet snippets (elixir-mode, elixir-ts-mode, org-mode)
init.el              # Entry point: bootstraps packages, loads config-elisp/
early-init.el        # Pre-init: sets library paths for native compilation (Nix/GCC)
minimal-config.el    # Loaded early: gcmh, key-chord, key-seq
readme.org           # Documentation index
```

**Gitignored directories** (do not check in): `straight/`, `elpa/`, `eln-cache/`,
`tree-sitter/`, `devdocs/`, `cache/`, `savefold/`, `undo-fu-session/`, `spell-fu/`,
`transient/`, `var/`, `backups/`

---

## Package Management

Two package managers are used in parallel — do not mix their usage patterns:

| Manager | Usage pattern | When to use |
|---|---|---|
| `package.el` + MELPA/ELPA | `use-package` with `:ensure t` | Standard packages available in MELPA/ELPA |
| `straight.el` | `straight-use-package` or `:straight (...)` in `use-package` | Git-pinned packages, forks, packages not in MELPA |

For built-in packages, use `:straight (:type built-in)` to prevent straight.el from
overriding them.

Author's own packages are loaded via `straight` from GitHub (e.g., `jasonmj/ex_tra`,
`jasonmj/org-toggl`).

---

## Key Binding Layers

There are three binding layers — changes must be considered at all three levels:

1. **Global bindings** — `global-set-key` / `emacs-set-key` (the `emacs-set-key` wrapper
   also registers EXWM simulation keys on Linux)
2. **Chord/sequence bindings** — `key-chord-define-global` / `key-seq-define-global`
   (requires `key-chord`, `key-seq`, `use-package-chords`)
3. **Modal bindings** — `modalka-define-kbd` in the `modalka-mode-map` (active when cursor
   is box-shaped / modal mode is on; toggle with `ii` or `s-i`)

---

## Elixir Configuration

Elixir has the deepest custom tooling in this config:

- **LSP**: `eglot` + `expert` LSP server (local build at `~/git/elixir-lang/expert/`),
  boosted with `eglot-booster`
- **Tree-sitter modes**: `elixir-ts-mode`, `heex-ts-mode` (primary); `elixir-mode` (fallback)
- **Test integration**: `flymake-mix-test` — custom Flymake backend parsing `mix test`
  output from shell buffers to show inline test failure diagnostics
- **IEx completion**: `cape-iex` — custom `cape` CAPF backend using `IEx.Autocomplete`
- **Refactoring**: `ex_tra` — author's own Elixir transform package

When modifying Elixir-related config, look in `config-org/elixir.org`.

---

## AI/ML Integration

This config has extensive AI tooling — relevant context when working on AI-related features:

- `agent-shell` — AI agent shell with auto-revert
- `opencode.el` — OpenCode AI integration
- `copilot.el` — GitHub Copilot completions (active in `elixir-ts-mode`)
- `gptel` — general-purpose LLM client (system message in `config-org/gptel-system-message.md`)
- `aidermacs`, `emigo` — AI pair programming tools
- `llm-utils.el` (tangled from `config-org/ai-ml.org`) — helpers providing Emacs context
  to LLMs: `llm-project-diff`, `llm-directory-tree`, `llm-file-preview`, `llm-code-outline`,
  `llm-symbol-search`, `llm-treesit-info`, `llm-xref-find-references`

---

## Completion Stack

The completion system ("MVCC stack") uses coordinated packages — treat as a unit:

- `vertico` + `vertico-posframe` — minibuffer completion in a floating frame
- `corfu` — in-buffer popup completion
- `orderless` — multi-regex completion style
- `prescient` + `vertico-prescient` + `corfu-prescient` — smart sorting
- `consult` — rich search/navigation
- `embark` + `embark-consult` — contextual action dispatch
- `cape` — completion-at-point extensions
- `marginalia` — minibuffer annotations
- `kind-icon` — LSP completion type icons

---

## Common Config-Org Files

| File | Covers |
|---|---|
| `config-org/appearance.org` | Themes, fonts, modeline, fringes |
| `config-org/completion.org` | Vertico, Corfu, Orderless, Cape, Consult, Embark |
| `config-org/elixir.org` | Elixir LSP, tree-sitter, test integration, IEx |
| `config-org/navigation.org` | Avy, modalka, windmove, zoom, bufler |
| `config-org/ai-ml.org` | Agent-shell, gptel, copilot, llm-utils |
| `config-org/shells.org` | Eshell, shell-mode, vterm, popper |
| `config-org/magit.org` | Magit, git-gutter, sideline-blame |
| `config-org/org.org` | Org-mode, org-toggl |

---

## No Test Suite

There is no automated test suite for this Emacs configuration. There are no `*-test.el`
files, ERT tests, or CI pipelines. Verification is manual — start Emacs and observe behavior.

---

## Platform Notes

- **macOS**: Primary. Uses `exec-path-from-shell`, command→Ctrl modifier remapping,
  `BetterTouchTool` desktop switching, transparent titlebar.
- **Linux/NixOS**: Secondary. EXWM window manager, Hyprland fallback for window movement,
  `battery-notifier`, `systemctl suspend`.
- `early-init.el` sets `LIBRARY_PATH`/`LD_LIBRARY_PATH` for Nix-provided `libgccjit`
  (native compilation on Apple Silicon + MacPorts GCC14).
