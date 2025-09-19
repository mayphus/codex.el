# codex.el

Minimal helpers for launching the Codex CLI from Emacs. Provides `codex-run`, which opens the Codex CLI in `vterm` when available or falls back to `ansi-term`.

## Usage

1. Install the file into your `load-path`.
2. Configure optional variables (`codex-cli-executable`, `codex-default-arguments`, `codex-buffer-name`).
3. Run `M-x codex-run` to launch or resume the Codex terminal buffer.

Requires Emacs 27.1+.
