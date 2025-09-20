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

(defcustom codex-buffer-name "*codex*"
  "Base name used when constructing Codex buffers."
  :type 'string
  :group 'codex)

(defun codex--normalize-directory (directory)
  "Return DIRECTORY without trailing slash, or nil.

This expands DIRECTORY so relative paths resolve predictably."
  (when directory
    (directory-file-name (expand-file-name directory))))

(defun codex--last-path-segment (directory)
  "Return the last segment of DIRECTORY or nil."
  (when directory
    (let ((resolved (codex--normalize-directory directory)))
      (unless (string-empty-p resolved)
        (file-name-nondirectory resolved)))))

(defun codex--sanitize-suffix (value)
  "Return VALUE with directory separators replaced by colons.

VALUE is trimmed of leading/trailing slashes.  Return nil for empty strings."
  (when value
    (let* ((trimmed (string-trim value "/" "/")))
      (unless (string-empty-p trimmed)
        (replace-regexp-in-string "/+" ":" trimmed)))))

(defun codex--project-root (directory)
  "Return project root for DIRECTORY or nil."
  (when (and directory (fboundp 'project-current) (fboundp 'project-root))
    (when-let ((project (project-current nil directory)))
      (ignore-errors
        (codex--normalize-directory (project-root project))))))

(defun codex--buffer-suffix-from-project (directory)
  "Return default buffer suffix using project metadata.

DIRECTORY must be normalized.  Return nil if no project context exists."
  (when-let* ((root (codex--project-root directory))
              (project-name (codex--last-path-segment root)))
    (let* ((relative (file-relative-name directory root))
           (relative-suffix (codex--sanitize-suffix relative)))
      (cond
       ((or (not relative-suffix) (string= relative-suffix ".")) project-name)
       (t (format "%s:%s" project-name relative-suffix))))))

(defun codex--path-segments (directory)
  "Return list of path segments for DIRECTORY.

DIRECTORY must be normalized."
  (when directory
    (split-string (directory-file-name directory) "/+" t)))

(defun codex--buffer-suffix-from-path (directory)
  "Return buffer suffix derived from DIRECTORY segments.

DIRECTORY must be normalized."
  (when-let* ((segments (codex--path-segments directory))
              (suffix-segments (if (>= (length segments) 2)
                                   (last segments 2)
                                 segments)))
    (mapconcat #'identity suffix-segments ":")))

(defun codex--buffer-suffix (directory)
  "Return suffix appended to the base buffer name for DIRECTORY."
  (or (codex--buffer-suffix-from-project directory)
      (codex--buffer-suffix-from-path directory)))

(defun codex--default-buffer-name (base-name directory)
  "Return BASE-NAME augmented with a suffix derived from DIRECTORY.

If DIRECTORY does not resolve to a meaningful segment, fall back to BASE-NAME."
  (let* ((normalized (codex--normalize-directory directory))
         (suffix (and normalized (codex--buffer-suffix normalized))))
    (if suffix
        (format "%s<%s>" base-name suffix)
      base-name)))

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
