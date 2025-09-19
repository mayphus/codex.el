;;; codex.el --- Minimal Codex terminal launcher -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Codex Agent
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; Provide a single interactive command that starts the Codex CLI inside a
;; real terminal emulator.  We prefer `vterm` when available, otherwise fall
;; back to the built-in `ansi-term`.
;;
;;; Code:

(require 'subr-x)
(require 'term)

(defgroup codex nil
  "Minimal helpers for launching the Codex CLI."
  :group 'external)

(defcustom codex-cli-executable "codex"
  "Executable used to launch Codex."
  :type 'string
  :group 'codex)

(defcustom codex-default-arguments nil
  "Default arguments passed to every Codex invocation."
  :type '(repeat string)
  :group 'codex)

(defcustom codex-buffer-name "*Codex*"
  "Name of the buffer used for Codex sessions."
  :type 'string
  :group 'codex)

(defun codex--executable ()
  "Return absolute path to the configured Codex executable."
  (or (executable-find codex-cli-executable)
      (user-error "Cannot find Codex executable %s" codex-cli-executable)))

(defun codex--command (extra-args)
  "Return list representing the Codex command with EXTRA-ARGS appended."
  (append (list (codex--executable))
          codex-default-arguments
          extra-args))

(defun codex--command-string (command)
  "Return shell-quoted string for COMMAND list."
  (mapconcat #'shell-quote-argument command " "))

(defun codex--read-extra-args ()
  "Read additional Codex arguments from the minibuffer."
  (let ((raw (string-trim (read-string "Codex arguments: "))))
    (if (string-empty-p raw)
        nil
      (split-string-and-unquote raw))))

(defun codex--terminal-backend ()
  "Select terminal backend symbol to use."
  (cond
   ((require 'vterm nil t) 'vterm)
   (t 'ansi-term)))

(defun codex--prepare-buffer ()
  "Remove stale Codex buffer so the name can be reused."
  (when-let* ((buffer (get-buffer codex-buffer-name))
              (process (get-buffer-process buffer)))
    (unless (process-live-p process)
      (kill-buffer buffer)))
  (when-let ((buffer (get-buffer codex-buffer-name)))
    (unless (get-buffer-process buffer)
      (kill-buffer buffer))))

(defun codex--switch-or-pop-buffer ()
  "Pop to existing Codex buffer when its process is alive.
Return non-nil when reuse happens."
  (when-let* ((buffer (get-buffer codex-buffer-name))
              (process (get-buffer-process buffer))
              ((process-live-p process)))
    (pop-to-buffer buffer)
    t))

(defun codex--start-with-vterm (command)
  "Launch Codex COMMAND using vterm."
  (unless (codex--switch-or-pop-buffer)
    (codex--prepare-buffer)
    (let ((vterm-shell (codex--command-string command)))
      (vterm codex-buffer-name))))

(defun codex--start-with-ansi-term (command)
  "Launch Codex COMMAND using ansi-term."
  (unless (codex--switch-or-pop-buffer)
    (codex--prepare-buffer)
    (let* ((binary (car command))
           (args (cdr command))
           (buffer (apply #'term-ansi-make-term
                          codex-buffer-name binary nil args)))
      (with-current-buffer buffer
        (term-mode)
        (term-char-mode))
      (pop-to-buffer buffer))))

;;;###autoload
(defun codex-run (&optional prefix)
  "Start or resume the Codex CLI buffer.

With PREFIX (\[universal-argument]), prompt for extra arguments appended to
`codex-default-arguments'."
  (interactive "P")
  (let* ((extra (when prefix (codex--read-extra-args)))
         (command (codex--command (or extra nil))))
    (pcase (codex--terminal-backend)
      ('vterm (codex--start-with-vterm command))
      (_ (codex--start-with-ansi-term command)))))

(provide 'codex)

;;; codex.el ends here
