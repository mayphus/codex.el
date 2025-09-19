;;; run-tests.el --- Run codex test suite -*- lexical-binding: t; -*-

(defconst codex-test-root
  (expand-file-name ".." (file-name-directory load-file-name))
  "Repository root for codex.el tests.")

(add-to-list 'load-path codex-test-root)
(add-to-list 'load-path (file-name-directory load-file-name))

(require 'codex)
(require 'codex-test)
(require 'checkdoc)

(let ((checkdoc-create-error-function
       (lambda (&rest args)
         (apply #'error args))))
  (checkdoc-file (expand-file-name "codex.el" codex-test-root)))

(require 'ert)
(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
