;;; benchmark-startup.el --- Isolated init profiler -*- lexical-binding: t; -*-

;; Run from the repository root:
;; emacs -Q --batch -l scripts/benchmark-startup.el
;; Optional environment variables:
;;   EMACS_BENCH_ROOT   Installed config/package directory (defaults to this repo).
;;   EMACS_BENCH_INIT   Init to measure (defaults to ROOT/init.el).
;;   EMACS_BENCH_EARLY  Early init to measure (defaults to ROOT/early-init.el).
;;   EMACS_BENCH_REPORT JSON output file (defaults to stdout).
;;   EMACS_BENCH_CHECK  Optional smoke-check Lisp file, loaded after timing.
;;
;; This measures full source init evaluation, not GUI launch/redisplay, restored
;; desktops, idle timers, or first-use latency.  Package installation, networking,
;; servers, and persistent user state are disabled. Straight runs in safe mode
;; against the installed tree, so builds/cache writes are excluded equally
;; from before/after results. Each run needs a fresh Emacs process.

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defvar my/benchmark-root
  (file-name-as-directory
   (expand-file-name
    (or (getenv "EMACS_BENCH_ROOT")
        "..")
    (file-name-directory (or load-file-name buffer-file-name)))))
(defvar my/benchmark-init
  (or (getenv "EMACS_BENCH_INIT")
      (expand-file-name "init.el" my/benchmark-root)))
(defvar my/benchmark-early
  (or (getenv "EMACS_BENCH_EARLY")
      (expand-file-name "early-init.el" my/benchmark-root)))
(defvar my/benchmark-temp (make-temp-file "jose-emacs-benchmark-" t))
(defvar my/benchmark-rows nil)
(defvar my/benchmark-warnings nil)
(defvar my/benchmark-errors nil)

(setq user-emacs-directory (file-name-as-directory my/benchmark-temp)
      user-init-file my/benchmark-init
      package-user-dir (expand-file-name "elpa" my/benchmark-root)
      straight-base-dir my/benchmark-root
      straight-safe-mode t
      straight-log nil
      native-comp-jit-compilation nil
      native-comp-deferred-compilation nil
      comp-enable-subr-trampolines nil
      custom-file (expand-file-name "custom.el" my/benchmark-temp)
      workflow-dir (expand-file-name "workflows" my/benchmark-temp)
      my/session-autosave-interval nil)

;; Keep paths expected by the init available without copying package code.
(make-symbolic-link (expand-file-name "straight" my/benchmark-root)
                    (expand-file-name "straight" user-emacs-directory))
(dolist (dir '("snippets" "scripts"))
  (make-symbolic-link (expand-file-name dir my/benchmark-root)
                      (expand-file-name dir user-emacs-directory)))

(defun my/benchmark-block-network (&rest _)
  (error "Network/package installation is disabled during startup benchmarks"))
(dolist (fn '(package-refresh-contents package-install package-vc-install
              url-retrieve url-retrieve-synchronously open-network-stream
              make-network-process))
  (advice-add fn :override #'my/benchmark-block-network))
(advice-add 'server-start :override #'ignore)
(advice-add 'server-running-p :override (lambda (&rest _) t))
(advice-add 'atomic-chrome-start-server :override #'ignore)

;; Cocoa-only frame chrome functions are absent from terminal-only builds.
(dolist (fn '(scroll-bar-mode tool-bar-mode tooltip-mode set-fringe-mode))
  (unless (fboundp fn) (fset fn #'ignore)))

(advice-add 'display-warning :before
            (lambda (type message &rest _)
              (push (format "%s: %s" type message) my/benchmark-warnings)))
(advice-add 'use-package-error :before
            (lambda (&rest args)
              (push (prin1-to-string args) my/benchmark-errors)))

(defun my/benchmark-load-init ()
  "Evaluate each source init form with normal lexical loading semantics."
  (let ((load-file-name my/benchmark-init)
        (load-in-progress t)
        (default-directory my/benchmark-root)
        (lexical-binding t))
    (with-temp-buffer
      (insert-file-contents my/benchmark-init)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let* ((line (line-number-at-pos))
                   (form (read (current-buffer)))
                   (label (if (eq (car-safe form) 'use-package)
                              (format "use-package %s" (cadr form))
                            (truncate-string-to-width
                             (replace-regexp-in-string
                              "[\n\t ]+" " " (prin1-to-string form)) 120)))
                   (started (current-time)))
              (eval form t)
              (push `((line . ,line) (form . ,label)
                      (seconds . ,(float-time
                                   (time-subtract (current-time) started))))
                    my/benchmark-rows)))
        (end-of-file nil)))))

(let ((started (current-time))
      (gcs-before gcs-done)
      (gc-time-before gc-elapsed)
      failure)
  (condition-case err
      (progn
        (load my/benchmark-early nil t t)
        (my/benchmark-load-init))
    (error (setq failure (error-message-string err))))
  (let ((seconds (float-time (time-subtract (current-time) started)))
        (gc-count (- gcs-done gcs-before))
        (gc-seconds (- gc-elapsed gc-time-before))
        (startup-features (sort (mapcar #'symbol-name features) #'string<)))
    (when-let* ((check (getenv "EMACS_BENCH_CHECK")))
      (condition-case err
          (load (expand-file-name check) nil t t)
        (error (setq failure (concat "Smoke check: "
                                    (error-message-string err))))))
    (let ((report
          `((emacs . ,emacs-version)
            (init . ,my/benchmark-init)
            (seconds . ,seconds)
            (garbage_collections . ,gc-count)
            (gc_seconds . ,gc-seconds)
            (feature_count . ,(length startup-features))
            (failure . ,failure)
            (errors . ,(vconcat (reverse my/benchmark-errors)))
            (warnings . ,(vconcat (reverse my/benchmark-warnings)))
            (features . ,(vconcat startup-features))
            (slowest_forms
             . ,(vconcat (seq-take
                          (sort my/benchmark-rows
                                (lambda (a b)
                                  (> (alist-get 'seconds a)
                                     (alist-get 'seconds b)))) 30))))))
    (if-let* ((file (getenv "EMACS_BENCH_REPORT")))
        (with-temp-file file (insert (json-encode report) "\n"))
      (princ (concat (json-encode report) "\n")))
    ;; Do not run state-saving hooks after the isolated directory is removed.
    (setq kill-emacs-hook nil confirm-kill-emacs nil)
    (delete-directory my/benchmark-temp t)
    (kill-emacs (if (or failure my/benchmark-errors) 1 0)))))
