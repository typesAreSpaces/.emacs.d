;;; check-startup.el --- Startup and first-use checks -*- lexical-binding: t; -*-
;; EMACS_BENCH_CHECK=$PWD/scripts/check-startup.el \
;;   emacs -Q --batch -l scripts/benchmark-startup.el

(require 'ert)

(ert-deftest my/startup-00-global-behavior ()
  (dolist (mode '(evil-mode persp-mode yas-global-mode global-corfu-mode
                 vertico-mode vertico-multiform-mode simpleclip-mode))
    (should (boundp mode))
    (should (symbol-value mode)))
  (should (eq (key-binding (kbd "C-s")) 'consult-line))
  (should (memq #'my/yasnippet-capf completion-at-point-functions))
  (dolist (feature '(org tex lsp-mode lsp-haskell typst-mode gptel jinx))
    (should-not (featurep feature))))

(ert-deftest my/startup-first-use ()
  ;; Client URLs must work even when no Org buffer has ever been opened.
  (my/load-org-protocol-for-client nil '(("org-protocol://test")))
  (should (featurep 'org-protocol))
  (should (advice-member-p #'org--protocol-detect-protocol-server
                           'server-visit-files))
  (with-temp-buffer
    (org-mode)
    (should org-indent-mode)
    (should visual-line-mode)
    (should (assoc "el" org-structure-template-alist))
    (should (eq org-agenda-window-setup 'current-window))
    (should (equal org-ellipsis "⇓")))
  ;; Load clients without starting external language servers.
  (require 'lsp-mode)
  (should (featurep 'lsp-haskell))
  (require 'tex)
  (should TeX-auto-save)
  (should TeX-parse-self)
  (should (autoloadp (symbol-function 'typst-mode)))
  (require 'typst-mode)
  (should (commandp 'typst-mode))
  (require 'typst-preview)
  (should (eq (lookup-key typst-preview-mode-map (kbd "C-c C-j"))
              'typst-preview-send-position))
  (require 'gptel)
  (should (equal gptel-model "gemma4:e2b"))
  (should (equal (gptel-backend-name gptel-backend) "Ollama")))

(let ((stats (ert-run-tests-batch "my/startup-")))
  (unless (zerop (ert-stats-completed-unexpected stats))
    (error "Startup regression checks failed")))
