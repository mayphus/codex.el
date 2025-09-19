# codex.el

Minimal helpers for launching the Codex CLI from Emacs. Provides `codex-run`,
which opens the Codex CLI inside a `vterm` buffer.

## Installation

For Emacs 29+ where `use-package` ships built in, you can install directly via
its `:vc` support:

```elisp
(use-package codex
  :vc (:url "https://github.com/mayphus/codex.el.git")
  :commands (codex-run))
```

Alternatively, clone the repository and add it to your `load-path` manually.

## Usage

1. Install the file into your `load-path`.
2. Ensure `vterm` is available; `codex-run` requires it.
3. Optionally set `codex-cli-executable`, `codex-default-arguments`, or
   `codex-buffer-name`.
4. Run `M-x codex-run` to launch or resume the Codex terminal buffer.

Requires Emacs 27.1+ and the `vterm` package.
