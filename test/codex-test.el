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

(ert-deftest codex-cleanup-buffer-removes-dead-buffer ()
  (let* ((codex-buffer-name "*codex-cleanup-test*")
         (buffer-name (codex--buffer-name))
         (buffer (get-buffer-create buffer-name)))
    (unwind-protect
        (progn
          (should (buffer-live-p buffer))
          (codex--cleanup-buffer)
          (should-not (buffer-live-p buffer)))
      (let ((live (get-buffer buffer-name)))
        (when live
          (kill-buffer live))))))

(ert-deftest codex-register-buffer-tracks-buffers ()
  (let ((codex--buffers nil)
        (buffer (get-buffer-create "*codex-track*")))
    (unwind-protect
        (progn
          (codex--register-buffer buffer)
          (should (equal codex--buffers (list buffer)))
          (kill-buffer buffer)
          (should-not codex--buffers))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest codex-switch-buffer-prompts-and-switches ()
  (let ((codex--buffers nil)
        (buf1 (get-buffer-create "*codex-switch-1*"))
        (buf2 (get-buffer-create "*codex-switch-2*")))
    (unwind-protect
        (progn
          (codex--register-buffer buf1)
          (codex--register-buffer buf2)
          (let (selected)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) (buffer-name buf1)))
                      ((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (setq selected buffer))))
              (codex-switch-buffer)
              (should (eq selected buf1))
              (should (eq (car codex--buffers) buf1)))))
      (mapc (lambda (buf)
              (when (buffer-live-p buf)
                (kill-buffer buf)))
            (list buf1 buf2)))))

(ert-deftest codex-switch-buffer-fails-without-buffers ()
  (let ((codex--buffers nil))
    (should-error (codex-switch-buffer) :type 'user-error)))

(provide 'codex-test)

;;; codex-test.el ends here
