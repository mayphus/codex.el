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

;;;###autoload
(defgroup codex nil
  "Minimal helpers for launching the Codex CLI."
  :group 'external)

;;;###autoload
(defcustom codex-cli-executable "codex"
  "Executable used to launch Codex."
  :type 'file
  :group 'codex)

;;;###autoload
(defcustom codex-default-arguments nil
  "Default arguments passed to every Codex invocation."
  :type '(repeat string)
  :group 'codex)

;;;###autoload
(defcustom codex-buffer-name "*codex*"
  "Base name used when constructing Codex buffers."
  :type 'string
  :group 'codex)

(defun codex--normalize-directory (directory)
  "Return DIRECTORY without trailing slash, or nil.

This expands DIRECTORY so relative paths resolve predictably."
  (when directory
    (directory-file-name (expand-file-name directory))))

(defun codex--abbreviate-directory (directory)
  "Return DIRECTORY abbreviated with a trailing slash."
  (when directory
    (file-name-as-directory (abbreviate-file-name directory))))

(defun codex--display-prefix (base-name)
  "Return BASE-NAME with surrounding asterisks removed."
  (if (and (> (length base-name) 1)
           (string-prefix-p "*" base-name)
           (string-suffix-p "*" base-name))
      (substring base-name 1 (1- (length base-name)))
    base-name))

(defun codex--default-buffer-name (base-name directory)
  "Return buffer name based on BASE-NAME and DIRECTORY.

If DIRECTORY does not resolve to a meaningful label, fall back to BASE-NAME."
  (let* ((normalized (codex--normalize-directory directory))
         (suffix (and normalized (codex--abbreviate-directory normalized))))
    (if suffix
        (let* ((prefix (codex--display-prefix base-name))
               (name (if (string-empty-p prefix) base-name prefix)))
          (format "%s: %s" name suffix))
      base-name)))

;;;###autoload
(defcustom codex-buffer-name-function #'codex--default-buffer-name
  "Function used to compute the Codex buffer name.

The function receives two arguments: the value of `codex-buffer-name' and the
`default-directory' at the time Codex is launched.  It must return the name of
an existing or new buffer."
  :type 'function
  :group 'codex)

(defun codex--buffer-name (&optional directory)
  "Compute the Codex buffer name for DIRECTORY.

If DIRECTORY is nil, use `default-directory'."
  (let ((dir (or directory default-directory)))
    (funcall codex-buffer-name-function codex-buffer-name dir)))

(defun codex--executable ()
  "Return absolute path to the configured Codex executable."
  (let* ((local-default
          (if (file-remote-p default-directory)
              (or (getenv "HOME") (expand-file-name "~"))
            default-directory))
         (local-path (let ((default-directory local-default))
                       (executable-find codex-cli-executable nil)))
         (remote-path (when (and (not local-path)
                                 (file-remote-p default-directory))
                        (executable-find codex-cli-executable t))))
    (or local-path remote-path
        (user-error "Cannot find Codex executable %s" codex-cli-executable))))

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

(defun codex--live-buffer (&optional directory)
  "Return live Codex buffer for DIRECTORY or nil."
  (when-let* ((buffer (get-buffer (codex--buffer-name directory)))
              (process (get-buffer-process buffer)))
    (when (process-live-p process)
      buffer)))

(defun codex--cleanup-buffer (&optional directory)
  "Remove stale Codex buffer for DIRECTORY if its process is dead."
  (when-let ((buffer (get-buffer (codex--buffer-name directory))))
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
         (buffer-name (codex--buffer-name))
         (live-buffer (codex--live-buffer)))
    (if live-buffer
        (pop-to-buffer live-buffer)
      (codex--cleanup-buffer)
      (let ((vterm-shell (codex--command-string command)))
        (vterm buffer-name)))))

(provide 'codex)

;;; codex.el ends here
