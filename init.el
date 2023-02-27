
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

                                        ; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-hide-results t)
  (auto-package-update-delete-old-versions t))

; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

; NOTE: If you want to move everything out of the (expand-file-name user-emacs-directory) folder
                                        ; reliably, set `user-emacs-directory` before loading no-littering!
                                        ; (setq user-emacs-directory "~/.cache/emacs")


(when (not (version< emacs-version "26.3"))
  (use-package no-littering))

                                        ; no-littering doesn't set this by default so we must place
                                        ; auto save files in the same path as it uses for sessions
(when (not (version< emacs-version "26.3"))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

; NOTE: init.el is now generated from config.org.  Please edit that file
                                        ;       in Emacs and init.el will be generated automatically!

                                        ; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 160)
(defvar efs/default-variable-font-size 160)

                                        ; Make frame transparency overridable
(defvar efs/frame-transparency '(100 . 100))

(defvar phd-thesis-dir "~/Documents/GithubProjects/phd-thesis")
(defvar ta-org-files-dir
  (concat phd-thesis-dir
          "/Documents/Semesters/2023/Spring/TA-CS-357/Org-Files"))
(defvar maxdiff-org-files-dir
  (concat phd-thesis-dir
          "/Documents/Side-Projects/MaxDiff/Documents/org"))
(defvar maxdiff-write-ups-dir
  (concat phd-thesis-dir
          "/Documents/Side-Projects/MaxDiff/Documents/notes"))

(defvar phd-thesis-write-ups-dir
  (concat phd-thesis-dir
          "/Documents/Write-Ups"))
(defvar phd-thesis-org-files-dir
  (concat phd-thesis-dir
          "/Documents/Org-Files"))

(defvar scc-dir
  (concat phd-thesis-dir
          "/Documents/Side-Projects/kapur-nsf-proposal"))
(defvar scc-reports-dir (concat scc-dir "/Reports"))
(defvar scc-org-files-dir (concat scc-dir "/Org-Files"))

(defvar seminar-dir
  (concat phd-thesis-dir
          "/Documents/Seminars/BeihangUniversity-Fall2021"))
(defvar seminar-org-files-dir (concat seminar-dir "/Org-Files"))
(defvar ta-tasks-mail
  (concat ta-org-files-dir "/current_tasks.org"))

(defvar maxdiff-agenda-mail
  (concat maxdiff-org-files-dir "/agenda.org"))

(defvar research-tasks-mail
  (concat phd-thesis-org-files-dir "/research_tasks.org"))
(defvar lunch-tasks-mail
  (concat phd-thesis-org-files-dir "/lunch_tasks.org"))
(defvar side-tasks-mail
  (concat phd-thesis-org-files-dir "/side_tasks.org"))
(defvar scc-tasks-mail
  (concat scc-org-files-dir "/scc_tasks.org"))
(defvar school-tasks-mail
  (concat phd-thesis-org-files-dir "/school_tasks.org"))
(defvar seminar-tasks-mail
  (concat seminar-org-files-dir "/seminar_tasks.org"))
(defvar seminar-meetings
  (concat seminar-org-files-dir "/meeting_notes.org"))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)               ; Disable visible scrollbar
(tool-bar-mode -1)                 ; Disable the toolbar
(tooltip-mode -1)                  ; Disable tooltips
(set-fringe-mode 10)               ; Give some breathing room

(menu-bar-mode -1)                 ; Disable the menu bar
(winner-mode 1)                    ; Enable winner mode
(setq winner-dont-bind-my-keys t)

(server-start)                     ; Start server
(setq process-connection-type nil) ; Use pipes
(setq history-length 25)
(savehist-mode 1)
(save-place-mode 1)
(setq use-dialog-box nil)

(setq ring-bell-function 'ignore)

(column-number-mode)
(setq-default display-line-numbers-type 'visual)
(when (not (version< emacs-version "26.3"))
  (global-display-line-numbers-mode t))

                                        ; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

                                        ; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                vterm-mode-hook
                markdown-mode-hook
                mu4e-headers-mode-hook
                mu4e-view-mode-hook
                mu4e-main-mode-hook
                mu4e-org-mode-hook
                mu4e-compose-mode-hook
                treemacs-mode-hook
                TeX-mode-hook
                LaTeX-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-to-list 'auto-mode-alist '("\\.dat\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.dat-s\\'" . text-mode))

(defvar dashboard-logo-path "~/Pictures/Wallpapers/figures/480px-EmacsIcon.svg.png")

(use-package all-the-icons)

(when (not (version< emacs-version "26.1"))
  (use-package dashboard
    :ensure t
    :config
                                        ;(setq dashboard-center-content t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-navigator t)
    (setq dashboard-banner-logo-title "Welcome to Emacs!")
    (when (file-exists-p dashboard-logo-path)
      (setq dashboard-startup-banner dashboard-logo-path))
    (setq dashboard-items '((recents  . 10)
                            (bookmarks . 10)
                            (projects . 5)))
    (dashboard-setup-startup-hook)))

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(defun frame-font-setup
    (&rest ...)
  ;; (remove-hook 'focus-in-hook #'frame-font-setup)
  (unless (assoc 'font default-frame-alist)
    (let* ((font-family (catch 'break
                          (dolist (font-family
                                   '("Fira Code"
                                     "Hack"
                                     "Consolas"))
                            (when (member font-family (font-family-list))
                              (throw 'break font-family)))))
           (font (when font-family (format "%s-18" font-family))))
      (when font
        (add-to-list 'default-frame-alist (cons 'font font))
        (set-frame-font font t t)))))

(add-hook 'focus-in-hook #'frame-font-setup)

; Make ESC quit prompts
(defun persp-exit ()
  (interactive)
  (prog1 (persp-state-save "~/.config/jose-emacs/.emacs-session") (save-buffers-kill-terminal)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key [(control x) (k)] 'kill-buffer)

(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
    "o" '(:ignore t :which-key "(o)rg")
    "oc" '(org-capture nil :which-key "org-(c)apture")
    "b" '(:ignore t :which-key "edit (b)uffer")
    "bc"  '(evilnc-comment-or-uncomment-lines :which-key "(c)omment line")
    "bf"  '(fill-buffer :which-key "(f)ill buffer")
    "bi"  '((lambda () (interactive)
              (indent-region (point-min) (point-max)))
            :which-key "(i)ndent buffer")
    "by" '(simpleclip-copy :which-key "clipboard (y)ank")
    "bp" '(simpleclip-paste :which-key "clipboard (p)aste")
    "f" '(:ignore t :which-key "edit (f)iles")
    "fa" '((lambda () (interactive)
             (find-file
              (expand-file-name (concat phd-thesis-org-files-dir "/main.org"))))
           :which-key "(a)genda")
    "fe" '((lambda () (interactive)
             (find-file
              (expand-file-name "config.org" user-emacs-directory)))
           :which-key "(e)macs source")
    "fw" '((lambda () (interactive)
             (find-file
              (expand-file-name
               (concat seminar-dir
                       "/Reports/finding_certificates_qm_univariate/main.tex"))))
           :which-key "Current (w)ork")
    "fr" '(:ignore t :which-key "Edit (r)eferences")
    "frp" '((lambda () (interactive)
              (find-file
               (expand-file-name (concat phd-thesis-write-ups-dir "/references.bib"))))
            :which-key "Edit (p)hD references")
    "frs" '((lambda () (interactive)
              (find-file
               (expand-file-name (concat scc-reports-dir "/references.bib"))))
            :which-key "Edit (s)CC references")
    "p" '(:ignore t :which-key "Presentation")
    "pp" '(org-tree-slide-move-previous-tree :which-key "Previous slide")
    "pn" '(org-tree-slide-move-next-tree  :which-key "Next slide")
    "s"  '(shell-command :which-key "(s)hell command")
    "t"  '(:ignore t :which-key "(t)oggles")
    "tt" '(load-theme :which-key "Choose (t)heme")
    "g" '(magit-status :which-key "Ma(g)it status")
    "d" '(dired-jump :which-key "(d)ired jump")
    "m" '(mu4e :which-key "(m)u4e")
    "l" '(:ignore t :which-key "(l)atex related")
    "lp" '((lambda () (interactive)
             (yasnippet/goto-parent-file))
           :which-key "Goto (p)arent")
    "lF" '((lambda () (interactive)
             (LaTeX-fill-buffer nil))
           :which-key "Latex (F)ill buffer")
    "lf" '((lambda () (interactive)
             (lsp-latex-forward-search))
           :which-key "Latex (f)orward search")
    "w" '(:ignore t :which-key "(w)indows related")
    "wu" '(winner-undo :which-key "Winner (u)ndo")
    "wr" '(winner-redo :which-key "Winner (r)edo")))

(use-package better-jumper
  :after evil
  :config
  (better-jumper-mode +1)
  (define-key evil-motion-state-map (kbd "C-i")
    'better-jumper-jump-forward)
  (define-key evil-motion-state-map (kbd "C-o")
    'better-jumper-jump-backward))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g")
    'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h")
    'evil-delete-backward-char-and-join)
                                        ; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(when (not (version< emacs-version "26.3"))
  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init)
    (setq forge-add-default-bindings nil)))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package command-log-mode
  :commands command-log-mode)

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

(use-package anzu)

(use-package evil-anzu
  :after evil
  :config (global-anzu-mode 1)
  (setq anzu-minimum-input-length 4))

(when (not (version< emacs-version "26.3"))
  (use-package doom-modeline
    :init (doom-modeline-mode 1)
    :custom (
             (doom-modeline-height 15)
             (doom-modeline-enable-word-count t)
             (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode text-mode)))))

;; (setq-default mode-line-format '("%e"
;;                                  (:eval
;;                                   (if (equal
;;                                        (shell-command-to-string
;;                                         "ps aux | grep 'mbsync -a' | wc -l | xargs")
;;                                        "3\n")
;;                                       " Running mbsync " " "))
;;                                  "%e" (:eval
;;                                        (when (display-graphic-p) (shell-command-to-string
;;                                                                   "~/.local/scripts/check_email.sh")))
;;                                  (:eval (doom-modeline-format--main))))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package flx)

(when (not (version< emacs-version "27.1"))
  (use-package marginalia
    ;; Either bind `marginalia-cycle` globally or only in the minibuffer
    :bind (("M-A" . marginalia-cycle)
           :map minibuffer-local-map
           ("M-A" . marginalia-cycle))

    ;; The :init configuration is always executed (Not lazy!)
    :init

    ;; Must be in the :init section of use-package such that the mode gets
    ;; enabled right away. Note that this forces loading the package.
    (marginalia-mode)))

(when (not (version< emacs-version "26.1"))
  (use-package embark
    :ensure t
    :bind
    (("C-." . embark-act)         ;; pick some comfortable binding
     ("C-;" . embark-dwim)        ;; good alternative: M-.
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)
    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (require 'embark)
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))))

                                        ; Consult users will also want the embark-consult package.
(when (not (version< emacs-version "27.1"))
  (use-package embark-consult
    :ensure t ; only need to install it, embark loads it after consult if found
    :after (embark consult)
    :demand t
    :hook
    (embark-collect-mode . consult-preview-at-point-mode)
    :init
    (with-eval-after-load 'embark
      (require 'embark-consult))))

(when (not (version< emacs-version "27.1"))
  (use-package vertico
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("C-h" . vertico-directory-delete-word))
    :init
    (vertico-mode)))

(when (not (version< emacs-version "26.1"))
  (use-package orderless
    :demand t
    :init
    ;; Configure a custom style dispatcher (see the Consult wiki)
    ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
    ;;       orderless-component-separator #'orderless-escapable-split-on-space)
    (setq completion-styles '(basic substring partial-completion orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion))))
    :config
    (setq orderless-matching-styles '(orderless-flex))))

(when (not (version< emacs-version "26.3"))
  (use-package consult
    :after (vertico perspective)
    :straight t
                                        ; Replace bindings. Lazily loaded due by `use-package'.
    :bind (; C-x bindings (ctl-x-map)
           ("C-x M-:" . consult-complex-command)     ; orig. repeat-complex-command
           ("C-x 4 b" . consult-buffer-other-window) ; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
           ("C-x r b" . consult-bookmark)            ; orig. bookmark-jump
           ("C-x p b" . consult-project-buffer)      ; orig. project-switch-to-buffer
                                        ; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store)          ; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)
                                        ; Other custom bindings
           ("M-y" . consult-yank-pop)                ; orig. yank-pop
           ("<help> a" . consult-apropos)            ; orig. apropos-command
                                        ; M-g bindings (goto-map)
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)               ; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)             ; orig. goto-line
           ("M-g M-g" . consult-goto-line)           ; orig. goto-line
           ("M-g o" . consult-outline)               ; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
                                        ; M-s bindings (search-map)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s L" . consult-line-multi)
           ("M-s m" . consult-multi-occur)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
                                        ; C-c bindings
           ("C-c C-b" . consult-buffer)                ; orig. switch-to-buffer
           ("C-s"     . consult-line)
           ("C-c C-f" . consult-find)
           ("C-c D" . consult-locate)
           ("C-c h" . consult-history)
           ("C-c m" . consult-mode-command)
           ("C-c k" . consult-kmacro)
           ("C-c C-g" . consult-grep)
                                        ; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)         ; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history)       ; orig. isearch-edit-string
           ("M-s l" . consult-line)                  ; needed by consult-line to detect isearch
           ("M-s L" . consult-line-multi)            ; needed by consult-line to detect isearch
                                        ; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history)                 ; orig. next-matching-history-element
           ("M-r" . consult-history))                ; orig. previous-matching-history-element

                                        ; Enable automatic preview at point in the *Completions* buffer. This is
                                        ; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)

                                        ; The :init configuration is always executed (Not lazy)
    :init

                                        ; Optionally configure the register formatting. This improves the register
                                        ; preview for `consult-register', `consult-register-load',
                                        ; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

                                        ; Optionally tweak the register preview window.
                                        ; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

                                        ; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

                                        ; Configure other variables and modes in the :config section,
                                        ; after lazily loading the package.
    :config
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source)
    (setq consult-project-root-function (lambda () (project-root (project-current))))
                                        ; Optionally configure preview. The default value
                                        ; is 'any, such that any key triggers the preview.
                                        ; (setq consult-preview-key 'any)
                                        ; (setq consult-preview-key (kbd "M-."))
                                        ; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
                                        ; For some commands and buffer sources it is useful to configure the
                                        ; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme
     :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-recent-file
     consult--source-project-recent-file
     :preview-key (kbd "M-."))

                                        ; Optionally configure the narrowing key.
                                        ; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ; (kbd "C-+")

                                        ; Optionally make narrowing help available in the minibuffer.
                                        ; You may want to use `embark-prefix-help-command' or which-key instead.
                                        ; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

                                        ; By default `consult-project-function' uses `project-root' from project.el.
                                        ; Optionally configure a different project root function.
                                        ; There are multiple reasonable alternatives to chose from.
                                        ; 1. project.el (the default)
                                        ; (setq consult-project-function #'consult--default-project--function)
                                        ; 2. projectile.el (projectile-project-root)
                                        ; (autoload 'projectile-project-root "projectile")
                                        ; (setq consult-project-function (lambda (_) (projectile-project-root)))
                                        ; 3. vc.el (vc-root-dir)
                                        ; (setq consult-project-function (lambda (_) (vc-root-dir)))
                                        ; 4. locate-dominating-file
                                        ; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
    ))

(defun consult-grep-from-here ()
  "Call `consult-grep' for the current buffer (a single file)."
  (interactive)
  (let ((consult-project-function (lambda (x) "./")))
    (consult-grep)))

(defun consult-find-from-here ()
  "Call `consult-find' for the current buffer (a single file)."
  (interactive)
  (let ((consult-project-function (lambda (x) "./")))
    (consult-find)))

(when (not (version< emacs-version "27.1"))
  (use-package citar
    :bind (("C-c b" . citar-insert-citation)
           :map minibuffer-local-map
           ("M-b" . citar-insert-preset))
    :custom
    (citar-bibliography `(,(concat scc-reports-dir "/references.bib")
                          ,(concat maxdiff-write-ups-dir "/references.bib")
                          ,(concat phd-thesis-write-ups-dir "/references.bib")))))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("q" nil "finished" :exit t))

(efs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(defun efs/org-font-setup ()
                                        ; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

                                        ; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Code" :weight 'regular :height (cdr face)))

                                        ; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (when (not (version< emacs-version "26.3"))
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch))
  (when (not (version< emacs-version "26.3"))
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)))

(when (not (version< (org-version) "9.2"))
  (with-eval-after-load 'org
                                        ; This is needed as of Org 9.2
    (require 'org-tempo)

    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-file-apps
        '((auto-mode . emacs)
          (directory . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.nb?\\'" . "Mathematica %s")
          ("\\.pdf\\'" . "zathura %s")))

  (setq org-ellipsis "⇓")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (require 'org-habit)
  (require 'org-protocol)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "EXTERNAL" "|")
          (sequence "GOAL" "IDEA" "OBSERVATION" "|")
          (sequence "TODAY" "TODO" "LATER" "|" "COMPLETED(c)" "CANC(k@)")
          (sequence "EMAIL" "|")))

                                        ; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

                                        ; Use find-file instead of file-find-other-window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

  (setq org-capture-templates
        `(
          ("e" "Email Capture")
          ("er" "Research Tasks" entry
           (file+olp research-tasks-mail "EMAIL")
           "** TODO Check this email %a"
           :immediate-finish t)
          ("el" "Lunch Tasks" entry
           (file+olp lunch-tasks-mail "EMAIL")
           "** TODO Check this email %a"
           :immediate-finish t)
          ("es" "S..")
          ("esc" "School Tasks" entry
           (file+olp school-tasks-mail "EMAIL")
           "** TODO Check this email %a"
           :immediate-finish t)
          ("est" "Seminar Tasks" entry
           (file+olp seminar-tasks-mail "EMAIL")
           "** TODO Check this email %a"
           :immediate-finish t)
          ("esm" "Seminar Meetings" plain
           (file+function seminar-meetings (lambda () (goto-line 5)))
           "%a"
           :prepend t
           :immediate-finish t)
          ("et" "TA Tasks" entry
           (file+olp ta-tasks-mail "EMAIL")
           "** TODO Check this email %a"
           :immediate-finish t)
          ("em" "MaxDiff Agenda" entry
           (file+olp maxdiff-agenda-mail "EMAIL")
           "** TODO Check this email %a"
           :immediate-finish t)
          )
        )

  (define-key org-mode-map (kbd "C-c c")
    (lambda () (interactive) (org-todo "COMPLETED")))

  (define-key org-mode-map (kbd "C-c t")
    (lambda () (interactive) (org-todo "TODO")))

  (define-key org-mode-map (kbd "C-c C-RET")
    (lambda () (interactive) (org-meta-return)))

  (define-key org-mode-map (kbd "C-c s")
    (lambda () (interactive) (org-sort-buffer)))

  (efs/org-font-setup))

(defun org-sort-buffer ()
  "Sort all entries in the current buffer, recursively."
  (interactive)
  (mark-whole-buffer)
  (org-sort-entries nil ?o)
  (org-map-entries (lambda ()
                     (condition-case x
                         (org-sort-entries nil ?o)
                       (user-error)))))

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             '("myarticle"
               "\\documentclass{article}
                  [NO-DEFAULT-PACKAGES]
                 \\usepackage{symbols}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("myreport"
               "\\documentclass[peerreview]{IEEEtran}
                  [NO-DEFAULT-PACKAGES]
                 \\usepackage{symbols}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(when (not (version< emacs-version "26.3"))
  (use-package ox-hugo
    :ensure t
    :pin melpa
    :after ox))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun fill-buffer ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (fill-region (point-min) (point-max)))))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook ((org-mode . efs/org-mode-visual-fill)
         (markdown-mode . efs/org-mode-visual-fill)
         (TeX-mode . efs/org-mode-visual-fill)
         (LaTeX-mode . efs/org-mode-visual-fill)
         (mu4e-main-mode . efs/org-mode-visual-fill)))

; Automatically tangle our config.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
                                        ; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package projectile
  :after orderless
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'orderless))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
                                        ; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/GithubProjects")
    (setq projectile-project-search-path '("~/Documents/GithubProjects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs `(,(expand-file-name "snippets" user-emacs-directory)))
  (setq yas-key-syntaxes '(yas-longest-key-from-whitespace "w_.()" "w_." "w_" "w"))
  (define-key yas-minor-mode-map (kbd "C-g") 'evil-normal-state)
  (define-key yas-keymap (kbd "C-g") 'evil-normal-state)
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(load (expand-file-name "snippets/yasnippet-scripts.el" user-emacs-directory))

(use-package perspective
  :ensure t
  :bind (("C-x k" . persp-kill-buffer*)
         ("C-x C-b" . consult-buffer))
  :custom
  (persp-mode-prefix-key (kbd "M-p"))
  :init
  (persp-mode))

(use-package avy
  :config
  (setq avy-all-windows 'all-frames)
  (global-set-key (kbd "C-:") 'avy-goto-char))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(when (not (version< emacs-version "26.1"))
  (use-package lsp-mode
    :commands (lsp lsp-deferred)
    :hook (lsp-mode . efs/lsp-mode-setup)
    :init
    (setq lsp-keymap-prefix "C-l")
    :config
    (lsp-enable-which-key-integration t)))

(when (not (version< emacs-version "26.1"))
  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom)))

; :hook (
                                        ; (org-mode TeX-mode LaTeX-mode typescript-mode
                                        ; maplev-mode c-mode c++-mode python-mode rustic-mode)
                                        ;. tree-sitter-hl-mode))

(when (fboundp 'module-load)
  (use-package tree-sitter
    :straight (tree-sitter :type git
                           :host github
                           :repo "ubolonton/emacs-tree-sitter"
                           :files ("lisp/*.el"))
    :hook ((latex-mode python-mode rustic-mode) . tree-sitter-hl-mode)
    :config
    (add-to-list 'tree-sitter-major-mode-language-alist '(rustic-mode . rust))
    (add-to-list 'tree-sitter-major-mode-language-alist '(TeX-mode . latex))
    (add-to-list 'tree-sitter-major-mode-language-alist '(LaTeX-mode . latex))
    (add-to-list 'tree-sitter-major-mode-language-alist '(latex-mode . latex))
    (add-to-list 'tree-sitter-major-mode-language-alist '(bibtex-mode . bibtex))
    (add-to-list 'tree-sitter-major-mode-language-alist '(org-mode . org))
    (add-to-list 'tree-sitter-major-mode-language-alist '(c-mode . c))
    (add-to-list 'tree-sitter-major-mode-language-alist '(cpp-mode . cpp))
    (add-to-list 'tree-sitter-major-mode-language-alist '(python-mode . python))
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . typescript))))

(when (fboundp 'module-load)
  (use-package tree-sitter-langs
    :straight (tree-sitter-langs :type git
                                 :host github
                                 :repo "ubolonton/emacs-tree-sitter"
                                 :files ("langs/*.el" "langs/queries"))
    :after tree-sitter))

(when (not (version< emacs-version "26.1"))
  (use-package treemacs
    :bind
    (:map global-map
          ([f4] . treemacs)
          ([f5] . treemacs-select-window))
    :config
    (setq treemacs-is-never-other-window t)))

(when (not (version< emacs-version "26.1"))
  (use-package treemacs-evil
    :after treemacs evil))

(when (not (version< emacs-version "26.1"))
  (use-package lsp-treemacs
    :after lsp))

(when (not (version< emacs-version "26.1"))
  (use-package dap-mode
                                        ; Uncomment the config below if you want all UI panes to be hidden by default!
                                        ; :custom
                                        ; (lsp-enable-dap-auto-configure nil)
                                        ; :config
                                        ; (dap-ui-mode 1)
    :commands dap-debug
    :config
                                        ; Set up Node debugging
    (require 'dap-node)
    (dap-node-setup) ; Automatically installs Node debug adapter if needed

                                        ; Bind `C-c l d` to `dap-hydra` for easy access
    (general-define-key
     :keymaps 'lsp-mode-map
     :prefix lsp-keymap-prefix
     "d" '(dap-hydra t :wk "debugger"))))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(add-hook 'TeX-mode-hook 'lsp)
(add-hook 'LaTeX-mode-hook 'lsp)

(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-hook 'TeX-mode-hook #'auto-fill-mode)
(add-hook 'LaTeX-mode-hook #'auto-fill-mode)
(setq-default fill-column 80)

(add-hook 'TeX-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'LaTeX-mode-hook #'display-fill-column-indicator-mode)

(when (not (version< emacs-version "26.1"))
  (use-package lsp-latex
    :bind (:map lsp-mode-map
                ("C-l w r" . lsp-workspace-restart)
                ("C-l w b" . lsp-latex-build))
    :config
                                        ; (setq lsp-completion-provider :none)
                                        ; (setq auto-complete-mode -1)
    (setq lsp-latex-build-executable "latexmk")
    (setq lsp-latex-build-args
          '("-pvc" "-pdf" "-interaction=nonstopmode" "-synctex=1" "-cd" "%f"))
    (setq lsp-latex-forward-search-after t)
    (setq lsp-latex-build-on-save t)
    (setq lsp-latex-forward-search-executable "zathura")
    (setq lsp-latex-forward-search-args '("--synctex-forward" "%l:1:%f" "%p"))))

(defun get-bibtex-from-doi (doi)
  "Get a BibTeX entry from the DOI"
  (interactive "MDOI: ")
  (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
    (with-current-buffer
        (url-retrieve-synchronously
         (format "http://dx.doi.org/%s"
                 (replace-regexp-in-string "http://dx.doi.org/" "" doi)))
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (setq bibtex-entry
            (buffer-substring
             (string-match "@" (buffer-string))
             (point)))
      (kill-buffer (current-buffer))))
  (insert (decode-coding-string bibtex-entry 'utf-8))
  (bibtex-fill-entry))

(when (not (version< emacs-version "26.3"))
  (use-package tex
    :ensure auctex
    :config
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)
    (setq reftex-ref-macro-prompt nil)
    (setq font-latex-fontify-script nil)))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
                                        ; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python)
  (setq python-indent-offset 2)
  (setq python-indent 2)
  (add-hook 'python-mode-hook
            (function (lambda ()
                        (setq indent-tabs-mode nil
                              tab-width 2)))))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package maplev
  :straight (maplev :type git
                    :host github
                    :repo "JoeRiel/maplev")
  :config
  (add-to-list 'auto-mode-alist '("\\.mpl\\'" . maplev-mode))
  (add-to-list 'auto-mode-alist '("\\.mm\\'" . maplev-mode)))

(use-package wolfram-mode
  :config
  (setq wolfram-program "/usr/local/bin/MathKernel")
  (setq wolfram-path "~/.Mathematica")
  (add-to-list 'auto-mode-alist '("\\.m\\'" . wolfram-mode))
  (add-to-list 'auto-mode-alist '("\\.wl\\'" . wolfram-mode)))

(use-package toml-mode)

(use-package lean4-mode
  :straight (lean4-mode :type git :host github :repo "leanprover/lean4-mode")
                                        ; to defer loading the package until required
  :commands (lean4-mode))

;(use-package racket-mode)
(setq scheme-program-name "/usr/bin/racket")
(setq auto-mode-alist
      (cons '("\\.rkt\\'" . scheme-mode)
            auto-mode-alist))

(defun run-scheme2 ()
  "Run scheme-program-name and disable geiser-mode."
  (interactive)
  (split-window-right)
  (geiser-mode -1)
  (windmove-right)
  (run-scheme scheme-program-name))

(defun run-scheme3 ()
  "Run scheme-program-name and disable geiser-mode."
  (interactive)
  (split-window-right)
  (windmove-right)
  (run-scheme scheme-program-name))

(use-package haskell-mode
  :mode "\\.hs\\'"
                                        ;:hook (haskell-mode . lsp-deferred)
  :config
  (setq haskell-program-name "/usr/bin/ghci")
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  ;; Choose indentation mode (the latter requires haskell-mode >= 2.5):
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  )
(use-package lsp-haskell)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(when (not (version< emacs-version "26"))
  (use-package company-box
    :hook (company-mode . company-box-mode)))

(when (not (version< emacs-version "26.3"))
  (use-package magit
    :commands magit-status
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)))

                                        ; NOTE: Make sure to configure a GitHub token before using this package!
                                        ; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
                                        ; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(when (not (version< emacs-version "26.3"))
  (use-package forge
    :after magit))

(use-package evil-nerd-commenter
  :after evil
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(when (not (version< emacs-version "26.3"))
  (use-package rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh") ; Change this to zsh, etc
                                        ;(setq explicit-zsh-args '())         ; Use 'explicit-<shell>-args for shell-specific args

                                        ; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun efs/configure-eshell ()
                                        ; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

                                        ; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

                                        ; Bind some useful keys for evil-mode
  (evil-define-key
    '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key
    '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(use-package dired
  :ensure nil
  :commands (dired dired-jump evil)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (when (not (version< emacs-version "26.3"))
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer)))

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(use-package dired-single
  :commands (dired dired-jump))

(when (not (version< emacs-version "26.1"))
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode)))

(use-package dired-open
  :commands (dired dired-jump)
  :config
                                        ; Doesn't work as expected!
                                        ;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package hide-mode-line)

(defun efs/presentation-setup ()
  (setq text-scale-mode-amount 3)
  (hide-mode-line-mode 1)
  (org-display-inline-images)
  (text-scale-mode 1))

(defun efs/presentation-end ()
  (hide-mode-line-mode 0)
  (text-scale-mode 0)
  (efs/org-mode-setup)
  (efs/org-mode-visual-fill))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . efs/presentation-setup)
         (org-tree-slide-stop . efs/presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " // ")
  (org-image-actual-width nil))

(use-package simpleclip
  :config
  (simpleclip-mode 1))

(use-package markdown-preview-eww
  :ensure nil
  :straight (
             :host github
             :files ("*.el")
             :repo "niku/markdown-preview-eww"))

(defvar efs/mu4e-path "/usr/share/emacs/site-lisp/mu4e/")

(when (file-exists-p (concat efs/mu4e-path "mu4e.el"))
  (use-package mu4e
    :ensure nil
    :load-path (lambda () (expand-file-name efs/mu4e-path))
                                        ; :defer 20 ; Wait until 20 seconds after startup
    :config
    (require 'mu4e)
    (require 'mu4e-org)

                                        ; This is set to 't' to avoid mail syncing issues when using mbsync
    (setq mu4e-change-filenames-when-moving t)

                                        ; SMTP settings
    (setq sendmail-program "/usr/bin/msmtp"
          message-sendmail-f-is-evil t
          message-sendmail-extra-arguments '("--read-envelope-from")
          send-mail-function 'smtpmail-send-it
          message-send-mail-function 'message-send-mail-with-sendmail)

    (setq smtpmail-debug-info t)
    (setq starttls-use-gnutls t)

    (setq mu4e-update-interval 600)
    (setq mu4e-get-mail-command "mbsync -a")
    (setq mu4e-root-maildir "~/Mail")

                                        ; Just plain text
    (with-eval-after-load "mm-decode"
      (add-to-list 'mm-discouraged-alternatives "text/html")
      (add-to-list 'mm-discouraged-alternatives "text/richtext"))

    (defun jcs-view-in-eww (msg)
      (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))
    (add-to-list 'mu4e-view-actions '("Eww view" . jcs-view-in-eww) t)

    (defun refile-func (msg)
      (cond
       ((mu4e-message-contact-field-matches msg :to "kapur@cs.unm.edu")
        "/unm/Prof. Kapur")
       ((mu4e-message-contact-field-matches msg :from "kapur@cs.unm.edu")
        "/unm/Prof. Kapur")
       ((mu4e-message-contact-field-matches msg :cc "kapur@cs.unm.edu")
        "/unm/Prof. Kapur")
       ((mu4e-message-contact-field-matches msg :to "kapur@unm.edu")
        "/unm/Prof. Kapur")
       ((mu4e-message-contact-field-matches msg :from "kapur@unm.edu")
        "/unm/Prof. Kapur")
       ((mu4e-message-contact-field-matches msg :cc "kapur@unm.edu")
        "/unm/Prof. Kapur")
       (t "/unm/Archive")))

    (setq mu4e-contexts
          (list
                                        ; School account
           (make-mu4e-context
            :name "School"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/unm" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address  . "jabelcastellanosjoo@unm.edu")
                    (user-full-name     . "Jose Abel Castellanos Joo")
                    (mu4e-drafts-folder . "/unm/Drafts")
                    (mu4e-sent-folder   . "/unm/Sent")
                    (mu4e-refile-folder . refile-func)
                    (mu4e-trash-folder  . "/unm/Trash")
                    (smtpmail-smtp-server . "smtp.office365.com")
                    (smtpmail-smtp-service . 587)
                    (smtpmail-debug-info . t)
                    (smtpmail-stream-type . starttls)))
                                        ; School CS department account
           (make-mu4e-context
            :name "CS department"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/cs-unm" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address  . "jose.castellanosjoo@cs.unm.edu")
                    (user-full-name     . "Jose Abel Castellanos Joo")
                    (mu4e-drafts-folder . "/cs-unm/Drafts")
                                        ;(mu4e-sent-folder   . "/cs-unm/Sent")
                    (mu4e-refile-folder . "/cs-unm/Inbox")
                    (mu4e-trash-folder  . "/cs-unm/Trash")
                    (smtpmail-smtp-server . "snape.cs.unm.edu")
                    (smtpmail-smtp-service . 1200)
                    (smtpmail-stream-type . starttls)))))

    (setq mu4e-context-policy 'pick-first)

    (setq mu4e-maildir-shortcuts
          '(("/unm/Inbox" . ?i)
            ("/unm/Sent"  . ?s)
            ("/unm/Trash" . ?t)
            ("/unm/Drafts". ?d)
            ("/unm/Prof. Kapur". ?k)
            ("/unm/Prof. Kapur/Side projects/Seminars/Beihang University". ?b)
            ("/unm/Prof. Kapur/Side projects/MaxDiff Extension". ?m)
            ("/unm/TA Work/CS 357". ?c)
            ("/unm/You got a Package!". ?p)
            ("/unm/Archive". ?a)
            ("/cs-unm/Inbox". ?I)
            ("/cs-unm/Trash". ?T)
            ("/cs-unm/Drafts". ?D)))))

                                        ; UX settings
(setq mu4e-use-fancy-chars t)
(setq mu4e-attachment-dir  "~/tosend")
(setq mu4e-headers-show-threads nil)
(setq mu4e-confirm-quit nil)
(setq mu4e-headers-results-limit -1)
(setq mu4e-compose-signature "Best,\nJose")
(setq message-citation-line-format "On %d %b %Y at %R, %f wrote:\n")
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq
                                        ; Display
 mu4e-view-show-addresses t
 mu4e-view-show-images t
 mu4e-view-image-max-width 800
 mu4e-hide-index-messages t)

(use-package org-mime
  :ensure t)

(load (expand-file-name "scripts/mu4e-view-save-all-attachments.el" user-emacs-directory))
