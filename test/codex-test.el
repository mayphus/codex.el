;;; codex-test.el --- Tests for codex.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'codex)

(ert-deftest codex-command-list-appends-extras ()
  (cl-letf (((symbol-function 'codex--executable)
             (lambda () "/usr/local/bin/codex"))
            (codex-default-arguments '("--json")))
    (should (equal (codex--command-list '("--verbose"))
                   '("/usr/local/bin/codex" "--json" "--verbose")))))

(ert-deftest codex-command-string-quotes-args ()
  (let* ((args '("codex" "--foo" "value with space"))
         (expected (mapconcat #'shell-quote-argument args " ")))
    (should (equal (codex--command-string args) expected))))

(provide 'codex-test)

;;; codex-test.el ends here
