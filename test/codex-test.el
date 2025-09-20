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

(ert-deftest codex-default-buffer-name-includes-directory ()
  (should (equal (codex--default-buffer-name "*codex*" "/tmp/my-app/")
                 (format "codex: %s"
                         (codex--abbreviate-directory "/tmp/my-app/"))))
  (should (equal (codex--default-buffer-name "*codex*" "/")
                 (format "codex: %s"
                         (codex--abbreviate-directory "/")))))

(ert-deftest codex-buffer-name-function-is-used ()
  (let* ((default-directory "/tmp/foo/")
         (codex-buffer-name "*base*")
         (codex-buffer-name-function
          (lambda (base dir)
            (format "%s:%s" base dir))))
    (should (equal (codex--buffer-name) "*base*:/tmp/foo/"))))

(provide 'codex-test)

;;; codex-test.el ends here
