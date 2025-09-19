# Contributing to codex.el

Thanks for your interest in improving codex.el! A few quick guidelines:

## Development Workflow

- Create topic branches from `main` for your changes.
- Keep commits focused and use concise imperative subject lines. Include a short
  body when the intent is not obvious from the diff.
- When updating behavior, add or adjust tests in the `test/` directory.

## Coding Style

- Follow the conventions used in `codex.el`: lexical binding, two-space
  indentation, and docstrings that start with an imperative verb.
- Run `make test` before opening a pull request to execute the automated
  checks.

## Release Process

- Update the `Version` header in `codex.el` when cutting a new release.
- Tag releases using semantic versioning (e.g., `v0.1.0`) so downstream users
  can pin to specific versions.

Feel free to open an issue or pull request if you have questions!
