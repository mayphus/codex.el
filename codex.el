;;; codex.el --- Minimal Codex terminal launcher -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Mayphus <tangmeifa@gmail.com>
;; Version: 0.1.1
;; Homepage: https://github.com/mayphus/codex.el
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1") (vterm "0.0"))

;;; Commentary:
;;
;; Provide a single interactive command that starts the Codex CLI inside a
;; `vterm` buffer.
;;
;;; Code:

(require 'subr-x)

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

(defun codex--command-list (extra-args)
  "Return command list with EXTRA-ARGS appended."
  (append (list (codex--executable))
          codex-default-arguments
          extra-args))

(defun codex--command-string (args)
  "Return shell-quoted string for ARGS list."
  (mapconcat #'shell-quote-argument args " "))

(defun codex--read-extra-args ()
  "Read additional Codex arguments from the minibuffer."
  (let ((raw (string-trim (read-string "Codex arguments: "))))
    (if (string-empty-p raw)
        nil
      (split-string-and-unquote raw))))

(defun codex--live-buffer ()
  "Return live Codex buffer or nil."
  (when-let* ((buffer (get-buffer codex-buffer-name))
              (process (get-buffer-process buffer))
              ((process-live-p process)))
    buffer))

(defun codex--cleanup-buffer ()
  "Remove stale Codex buffer if its process is dead."
  (when-let ((buffer (get-buffer codex-buffer-name)))
    (unless (and (get-buffer-process buffer)
                 (process-live-p (get-buffer-process buffer)))
      (kill-buffer buffer))))


;;;###autoload
(defun codex (&optional prefix)
  "Start or resume the Codex CLI buffer.

With PREFIX (\[universal-argument]), prompt for extra arguments appended to
`codex-default-arguments'."
  (interactive "P")
  (unless (require 'vterm nil t)
    (user-error "Codex.el requires vterm"))
  (let* ((extra (when prefix (codex--read-extra-args)))
         (command (codex--command-list (or extra nil)))
         (live-buffer (codex--live-buffer)))
    (if live-buffer
        (pop-to-buffer live-buffer)
      (codex--cleanup-buffer)
      (let ((vterm-shell (codex--command-string command)))
        (vterm codex-buffer-name)))))

(provide 'codex)

;;; codex.el ends here
