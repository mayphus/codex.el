;;; run-tests.el --- Run codex test suite -*- lexical-binding: t; -*-

(defconst codex-test-root
  (expand-file-name ".." (file-name-directory load-file-name))
  "Repository root for codex.el tests.")

(add-to-list 'load-path codex-test-root)
(add-to-list 'load-path (file-name-directory load-file-name))

(require 'cl-lib)
(require 'codex)
(require 'codex-test)
(require 'checkdoc)

(let ((checkdoc-create-error-function
       (lambda (&rest args)
         (apply #'error args))))
  (checkdoc-file (expand-file-name "codex.el" codex-test-root)))

(let ((package-lint-exit-code 0)
      (command-line-args-left
       (list (expand-file-name "codex.el" codex-test-root))))
  (when (require 'package-lint nil t)
    ;; Intercept package-lint's call to `kill-emacs' so ERT still runs.
    (cl-letf (((symbol-function 'kill-emacs)
               (lambda (&optional code)
                 (setq package-lint-exit-code (or code 0)))))
      (package-lint-batch-and-exit)))
  (unless (zerop package-lint-exit-code)
    (kill-emacs package-lint-exit-code)))

(require 'ert)
(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
