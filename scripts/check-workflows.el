;;; check-workflows.el --- Isolated workflow regression checks -*- lexical-binding: t; -*-
;; Run: emacs -Q --batch -l scripts/check-workflows.el
(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'desktop)
(require 'tab-bar)
(setq native-comp-enable-subr-trampolines nil
      comp-enable-subr-trampolines nil)

;; Evaluate the actual workflow configuration without starting packages, servers,
;; or the user's startup hooks.  Every desktop read/write uses a temporary tree.
(defvar my/workflow-test-root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))
(with-temp-buffer
  (insert-file-contents (expand-file-name "init.el" my/workflow-test-root))
  (goto-char (point-min))
  (let (form)
    (while (not (and (eq (car-safe (setq form (read (current-buffer)))) 'use-package)
                     (eq (cadr form) 'desktop))))
    (eval (cons 'progn (cdddr form)) t)))

(let ((load-path (cons (car (file-expand-wildcards
                           (expand-file-name "elpa/perspective-*" my/workflow-test-root)))
                       load-path)))
  (require 'perspective))

(setq persp-mode-prefix-key (kbd "M-p"))

(defvar test-dir)

(defmacro my/with-workflow-test (&rest body)
  `(let* ((test-dir (make-temp-file "jose-workflow-test-" t))
          (workflow-dir (expand-file-name "workflows" test-dir))
          (my/last-session-file (expand-file-name "last" test-dir))
          (desktop-dirname nil)
          (desktop-file-modtime nil)
          (desktop-buffer-args-list nil)
          (desktop-lazy-timer nil)
          (desktop-restore-eager 0)
          (tab-bar-new-tab-choice "*scratch*")
          (tab-bar-new-tab-to 'rightmost))
     (unwind-protect
         (progn (make-directory workflow-dir t)
                (tab-bar-mode 1)
                (tab-bar-close-other-tabs)
                ,@body)
       (desktop-lazy-abort)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer))
                     ((file-in-directory-p file test-dir)))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (tab-bar-close-other-tabs)
       (delete-directory test-dir t))))

(defun my/workflow-test-file (name)
  (let ((file (expand-file-name name test-dir)))
    (make-directory (file-name-directory file) t)
    (write-region "first line\nsecond line\nthird line\n" nil file nil 'silent)
    (find-file-noselect file)))

(ert-deftest my/workflow-roundtrip-all-buffers-and-layouts ()
  (my/with-workflow-test
   (let ((one (my/workflow-test-file "one.txt"))
         (two (my/workflow-test-file "two.txt"))
         (hidden (my/workflow-test-file "hidden.txt")))
     (switch-to-buffer one)
     (goto-char 15)
     (split-window-right)
     (set-window-buffer (next-window) two)
     (tab-bar-rename-tab "split")
     (tab-bar-new-tab)
     (switch-to-buffer two)
     (tab-bar-rename-tab "second")
     (my-save-session "roundtrip")
     (should (equal (desktop-owner) (emacs-pid)))
     (let ((noninteractive nil)) (my-load-session "roundtrip"))
     (should-not (buffer-live-p hidden))
     (should (get-file-buffer (expand-file-name "hidden.txt" test-dir)))
     (should-not desktop-buffer-args-list)
     (should (equal (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs))
                    '("split" "second")))
     (should (= (tab-bar--current-tab-index) 1))
     (should (equal (buffer-file-name (window-buffer))
                    (expand-file-name "two.txt" test-dir)))
     (tab-bar-select-tab 1)
     (should (= (length (window-list)) 2))
     (should (= (window-point) 15)))))

(ert-deftest my/workflow-roundtrip-keeps-automatic-tab-names ()
  (my/with-workflow-test
   (tab-bar-rename-tab "")
   (switch-to-buffer (my/workflow-test-file "one.txt"))
   (tab-bar-new-tab)
   (switch-to-buffer (my/workflow-test-file "two.txt"))
   (tab-bar-rename-tab "named")
   (my-save-session "automatic")
   (let ((noninteractive nil)) (my-load-session "automatic"))
   (tab-bar-select-tab 1)
   (should-not (alist-get 'explicit-name (tab-bar--current-tab)))
   (switch-to-buffer (get-file-buffer (expand-file-name "two.txt" test-dir)))
   (should (equal (alist-get 'name (tab-bar--current-tab)) "two.txt"))
   (tab-bar-select-tab 2)
   (should (alist-get 'explicit-name (tab-bar--current-tab)))
   (should (equal (alist-get 'name (tab-bar--current-tab)) "named"))))

(ert-deftest my/workflow-save-as-transfers-lock ()
  (my/with-workflow-test
   (switch-to-buffer (my/workflow-test-file "one.txt"))
   (my-save-session "one")
   (my-save-session "two")
   (should-not (desktop-owner (expand-file-name "one" workflow-dir)))
   (should (equal (desktop-owner (expand-file-name "two" workflow-dir)) (emacs-pid)))))

(ert-deftest my/workflow-load-abandoned-lock ()
  (my/with-workflow-test
   (switch-to-buffer (my/workflow-test-file "one.txt"))
   (my-save-session "one")
   (write-region "99999999" nil (desktop-full-lock-name) nil 'silent)
   (cl-letf (((symbol-function 'desktop--emacs-pid-running-p) (lambda (_) nil)))
     (let ((noninteractive nil)) (my-load-session "one")))
   (should (equal (desktop-owner) (emacs-pid)))))

(ert-deftest my/workflow-invalid-tabs-keeps-current-buffers ()
  (my/with-workflow-test
   (let ((one (my/workflow-test-file "one.txt")))
     (switch-to-buffer one)
     (my-save-session "one")
     (with-temp-file (expand-file-name "one/tabs.el" workflow-dir)
       (prin1 '(:version 2 :selected 9 :tabs ((:name "bad" :state (bad))))
              (current-buffer)))
     (should-error (my-load-session "one") :type 'user-error)
     (should (buffer-live-p one))
     (should (equal (my/current-session) "one")))))

(ert-deftest my/workflow-live-lock-keeps-current-buffers ()
  (my/with-workflow-test
   (let ((one (my/workflow-test-file "one.txt")))
     (switch-to-buffer one)
     (my-save-session "one")
     (write-region "99999999" nil (desktop-full-lock-name) nil 'silent)
     (cl-letf (((symbol-function 'desktop--emacs-pid-running-p) (lambda (_) t)))
       (should-error (my-load-session "one") :type 'user-error))
     (should (buffer-live-p one)))))

(ert-deftest my/workflow-failed-read-detaches ()
  (my/with-workflow-test
   (switch-to-buffer (my/workflow-test-file "one.txt"))
   (my-save-session "one")
   (cl-letf (((symbol-function 'my/read-session-desktop)
              (lambda (dir) (setq desktop-dirname dir) (error "Broken desktop"))))
     (should-error (my-load-session "one")))
   (should-not (my/current-session))
   (should-not (desktop-owner (expand-file-name "one" workflow-dir)))))

(ert-deftest my/workflow-save-conflict-does-not-rewrite-tabs ()
  (my/with-workflow-test
   (switch-to-buffer (my/workflow-test-file "one.txt"))
   (my-save-session "one")
   (let* ((tabs-file (expand-file-name "one/tabs.el" workflow-dir))
          (before (with-temp-buffer (insert-file-contents tabs-file) (buffer-string))))
     (set-file-times (desktop-full-file-name) (time-add (current-time) 60))
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
       (should-error (my-save-session "one")))
     (should (equal before (with-temp-buffer
                            (insert-file-contents tabs-file) (buffer-string)))))))

(ert-deftest my/workflow-perspective-last-buffer ()
  (my/with-workflow-test
   (unwind-protect
       (progn
         (persp-mode 1)
         (persp-switch "workflow-test")
         (let ((one (my/workflow-test-file "perspective.txt")))
           (switch-to-buffer one)
           (setf (persp-current-buffers) (list one))
           (my-save-session "one")
           (let ((noninteractive nil)) (my-load-session "one"))
           (should-not (buffer-live-p one))
           (should (get-file-buffer (expand-file-name "perspective.txt" test-dir)))))
     (persp-mode -1))))

(ert-deftest my/workflow-pending-buffers-survive-save ()
  (my/with-workflow-test
   (switch-to-buffer (my/workflow-test-file "pending.txt"))
   (my-save-session "one")
   (my/clear-session)
   ;; Simulate a desktop loaded by the previous lazy configuration.
   (let ((noninteractive nil))
     (desktop-read (expand-file-name "one" workflow-dir)))
   (should desktop-buffer-args-list)
   (my-save-session "one")
   (should-not desktop-buffer-args-list)
   (let ((noninteractive nil)) (my-load-session "one"))
   (should (get-file-buffer (expand-file-name "pending.txt" test-dir)))))

(ert-deftest my/workflow-tab-error-detaches ()
  (my/with-workflow-test
   (switch-to-buffer (my/workflow-test-file "one.txt"))
   (my-save-session "one")
   (cl-letf (((symbol-function 'window-state-put) (lambda (&rest _) (error "Bad state"))))
     (let ((noninteractive nil)) (should-error (my-load-session "one"))))
   (should-not (my/current-session))
   (should-not (desktop-owner (expand-file-name "one" workflow-dir)))))

(ert-deftest my/workflow-refused-close-keeps-saved-desktop ()
  (my/with-workflow-test
   (let ((one (my/workflow-test-file "one.txt")))
     (switch-to-buffer one)
     (my-save-session "one")
     (with-current-buffer one
       (add-hook 'kill-buffer-query-functions (lambda () nil) nil t))
     (unwind-protect
         (progn
           (should-error (my-load-session "one") :type 'user-error)
           (should (buffer-live-p one))
           (should-not (my/current-session))
           (should-not (desktop-owner (expand-file-name "one" workflow-dir))))
       (with-current-buffer one (setq kill-buffer-query-functions nil))))))

(ert-deftest my/workflow-rej-starts-unsaved-then-loads-existing ()
  (my/with-workflow-test
   (switch-to-buffer (my/workflow-test-file "learning.txt"))
   (my-save-session "learning")
   (my/clear-session)
   (let ((my/start-new-session t)
         (noninteractive nil))
     ;; Batch Emacs has an initial terminal, unlike the daemon's client frame.
     (cl-letf (((symbol-function 'my/session-frame-p) (lambda (&optional _) t)))
       (my/restore-last-session))
     (should-not my/start-new-session))
   (should-not (my/current-session))
   (should (equal (my/session-names) '("learning")))
   (should (equal (my/last-session) "learning"))
   (should (= (length (tab-bar-tabs)) 1))
   ;; Change must not save the unnamed workspace before loading the target.
   (let ((noninteractive nil)) (my-change-session "learning"))
   (should (equal (my/current-session) "learning"))
   (should (get-file-buffer (expand-file-name "learning.txt" test-dir)))
   (should (equal (my/session-names) '("learning")))))

(ert-deftest my/workflow-explicit-new-still-saves ()
  (my/with-workflow-test
   (let ((my/session-counter 0))
     (my-new-session)
     (should (equal (my/current-session) "session-1"))
     (should (equal (my/session-names) '("session-1")))
     (should (file-exists-p (desktop-full-file-name)))
     (should (file-exists-p (expand-file-name "session-1/tabs.el" workflow-dir))))))

(ert-run-tests-batch-and-exit "my/workflow-")
