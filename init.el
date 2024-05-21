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

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq evil-want-keybinding nil)

(use-package auto-package-update
  :custom
  (auto-package-update-hide-results t)
  (auto-package-update-delete-old-versions t))

(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setq gc-cons-threshold (* 2 1000 1000))

(when (not (version< emacs-version "26.3"))
  (use-package no-littering))

(when (not (version< emacs-version "26.3"))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(defvar phd-thesis-dir "~/Documents/GithubProjects/phd-thesis")
(defvar website-dir "~/Documents/GithubProjects/website")
(defvar website-posts (concat website-dir "/content-org/all-posts.org"))
(defvar current-semester-dir
  (concat phd-thesis-dir
          "/Documents/Semesters/2023/Fall"))
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
          "/Documents/Side-Projects/kapur-nsf-proposal/2022"))
(defvar scc-reports-dir (concat scc-dir "/Reports"))
(defvar scc-org-files-dir (concat scc-dir "/Org-Files"))

(defvar seminar-dir
  (concat phd-thesis-dir
          "/Documents/Seminars/BeihangUniversity-Fall2021"))
(defvar seminar-org-files-dir (concat seminar-dir "/Org-Files"))
(defvar ta1-tasks-mail
  (concat current-semester-dir "/TA-CS-105/Org-Files/current_tasks.org"))
(defvar ta2-tasks-mail
  (concat current-semester-dir "/TA-CS-561/Org-Files/current_tasks.org"))

(defvar maxdiff-agenda-mail
  (concat maxdiff-org-files-dir "/agenda.org"))

(defvar agenda-mail
  (concat phd-thesis-org-files-dir "/main.org"))
(defvar research-tasks-mail
  (concat phd-thesis-org-files-dir "/research_tasks.org"))
(defvar dissertation-tasks-mail
  (concat phd-thesis-org-files-dir "/dissertation_tasks.org"))
(defvar graduation-logistics-tasks-mail
  (concat phd-thesis-org-files-dir "/graduation_logistics.org"))
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

(defvar efs/frame-transparency '(100 . 100))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)               ; Disable visible scrollbar
(tool-bar-mode -1)                 ; Disable the toolbar
(tooltip-mode -1)                  ; Disable tooltips
(set-fringe-mode 10)               ; Give some breathing room

(menu-bar-mode -1)                 ; Disable the menu bar
(setq make-backup-files nil)
(winner-mode 1)                    ; Enable winner mode
(setq winner-dont-bind-my-keys t)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(server-start)                     ; Start server
(setq process-connection-type nil) ; Use pipes
(setq history-length 25)
(savehist-mode 1)
(save-place-mode 1)
(setq use-dialog-box nil)

(setq ring-bell-function 'ignore)
(setq confirm-kill-emacs #'y-or-n-p)

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
(dolist (mode '(term-mode-hook
                shell-mode-hook
                vterm-mode-hook
                mu4e-headers-mode-hook
                mu4e-view-mode-hook
                mu4e-main-mode-hook
                mu4e-org-mode-hook
                mu4e-compose-mode-hook
                treemacs-mode-hook
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
                                        ; (setq dashboard-center-content t)
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

(setq tab-bar-show 1)                      ; hide bar if <= 1 tabs open
(setq tab-bar-new-tab-choice "*dashboard*"); buffer to show in new tabs
(setq tab-bar-tab-hints t)                 ; show tab numbers
(setq tab-bar-new-tab-to 'rightmost)       ; defines where to create a new tab
(set-face-attribute 'tab-bar nil
                    :background "#282828"
                    :foreground "gray60" :distant-foreground "gray50"
                    :height 1.0 :box nil)
(set-face-attribute 'tab-bar-tab nil
                    :background "#B8BB26"
                    :foreground "black" :distant-foreground "gray60"
                    :height 1.0 :box nil)
(set-face-attribute 'tab-bar-tab-inactive nil
                    :background "#282828"
                    :foreground "white" :distant-foreground "gray50"
                    :height 1.0 :box nil)

(set-face-attribute 'tab-line nil ; background behind tabs
                    :background "gray40"
                    :foreground "gray60" :distant-foreground "gray50"
                    :height 1.0 :box nil)
                                        ; (set-face-attribute 'tab-line-tab nil ; active tab in another window
                                        ;                    :inherit 'tab-line
                                        ;                    :foreground "gray70" :background "gray90" :box nil)
                                        ; (set-face-attribute 'tab-line-tab-current nil ; active tab in current window
                                        ;                     :background "#b34cb3" :foreground "white" :box nil)
                                        ; (set-face-attribute 'tab-line-tab-inactive nil ; inactive tab
                                        ;                    :background "gray60" :foreground "black" :box nil)
                                        ; (set-face-attribute 'tab-line-highlight nil ; mouseover
                                        ;                    :background "white" :foreground 'unspecified)

(defun frame-font-setup
    (&rest ...)
                                        ; (remove-hook 'focus-in-hook #'frame-font-setup)
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

(defun change-font-size (size)
  (interactive "n")
  (set-face-attribute 'default nil :height size))

(defun toggle-zoom-pane ()
  (interactive)
  (if (get 'is-pane-zoomed 'state)
      (progn
        (winner-undo)
        (setq mode-line-misc-info "")
        (put 'is-pane-zoomed 'state nil))
    (progn
      (delete-other-windows)
      (setq mode-line-misc-info "[\ueb81]")
      (put 'is-pane-zoomed 'state t))))

(define-key (current-global-map) (kbd "C-w") nil)
(define-key (current-global-map) (kbd "C-w z") 'toggle-zoom-pane)

(defun persp-exit ()
  (interactive)
  (prog1
      (persp-state-save "~/.config/jose-emacs/.emacs-session-mac")
    (save-buffers-kill-terminal)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key [(control x) (k)] 'kill-buffer)

                                        ; Unbind C-@ in order to make it a global-prefix for general
(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "C-@"))

(when (eq system-type 'darwin) ; mac specific settings
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
    "a" '(:ignore t :which-key "(a)vy")
    "ac" '(avy-goto-char :which-key "(c)haracter")
    "aw" '(avy-goto-word-0 :which-key "(w)ord")
    "b" '(:ignore t :which-key "(b)ookmark")
    "bs" '(bookmark-set :which-key "bookmark (s)et")
    "bb" '(consult-bookmark :which-key "(b)ookmark jump")
    "bd" '(bookmark-delete :which-key "bookmark (d)elete")
    "e" '(:ignore t :which-key "(e)dit buffer")
    "ec"  '(evilnc-comment-or-uncomment-lines :which-key "(c)omment line")
    "ef"  '(fill-paragraph :which-key "(f)ill paragraph")
    "ei"  '((lambda () (interactive)
              (indent-region (point-min) (point-max)))
            :which-key "(i)ndent buffer")
    "ey" '(simpleclip-copy :which-key "clipboard (y)ank")
    "es" '(insert-snake :which-key "insert (s)nake")
    "ep" '(simpleclip-paste :which-key "clipboard (p)aste")
    "f" '(hydra-jump-files/body :which-key "edit (f)iles")
    "s"  '(shell-command :which-key "(s)hell command")
    "S"  '(async-shell-command :which-key "async (S)hell command")
    "t"  '(:ignore t :which-key "(t)oggles/(t)abs")
    "tt" '(load-theme :which-key "choose (t)heme")
    "ts" '(tab-switch :which-key "(s)witch tab")
    "td" '(tab-duplicate :which-key "tab (d)uplicate")
    "tn" '(tab-new :which-key "(n)ew tab")
    "tc" '(tab-close :which-key "(c)lose tab")
    "th" '(tab-previous :which-key "move to left tab")
    "tl" '(tab-next :which-key "move to right tab")
    "tr" '(tab-rename :which-key "(r)ename tab")
    "g" '(magit-status :which-key "Ma(g)it status")
    "d" '(dired-jump :which-key "(d)ired jump")
    "m" '(mu4e :which-key "(m)u4e")
    "w" '(:ignore t :which-key "(w)indows related")
    "wz" '(toggle-zoom-pane :which-key "Zoom toggle")
    "wu" '(winner-undo :which-key "Winner (u)ndo")
    "wr" '(winner-redo :which-key "Winner (r)edo")))

(use-package better-jumper
  :after (evil god-mode)
  :custom
                                        ; ; this is the key to avoiding conflict with evils jumping stuff
  (better-jumper-use-evil-jump-advice t)

  :config
  (better-jumper-mode +1)
                                        ; this lets me toggle between two points. (adapted from evil-jump-backward-swap)
  (evil-define-motion better-jumper-toggle (count)
    (let ((pnt (point)))
      (better-jumper-jump-backward 1)
      (better-jumper-set-jump pnt)))

                                        ; this is the key here. This advice makes it so you only set a jump point
                                        ; if you move more than one line with whatever command you call. For example
                                        ; if you add this advice around evil-next-line, you will set a jump point
                                        ; if you do 10 j, but not if you just hit j. I did not write this code, I
                                        ; I found it a while back and updated it to work with better-jumper.
  (defun my-jump-advice (oldfun &rest args)
    (let ((old-pos (point)))
      (apply oldfun args)
      (when
          (>
           (abs
            (-
             (line-number-at-pos old-pos)
             (line-number-at-pos (point))))
           1)
        (better-jumper-set-jump old-pos))))
  (define-key god-local-mode-map (kbd "o") 'better-jumper-jump-backward)
  (define-key god-local-mode-map (kbd "u") 'better-jumper-jump-forward)
  (define-key evil-motion-state-map (kbd "C-u")
              'better-jumper-jump-forward)
  (define-key evil-motion-state-map (kbd "C-o")
              'better-jumper-jump-backward))

                                        ; jump scenarios
(advice-add 'evil-next-line :around #'my-jump-advice)
(advice-add 'evil-previous-line :around #'my-jump-advice)
(advice-add 'evil-goto-definition :around #'my-jump-advice)
(advice-add 'evil-goto-mark  :around #'my-jump-advice)

(use-package god-mode
  :config
  (global-set-key (kbd "s-g") #'god-mode-all)
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  (global-set-key
   (kbd "C-g")
   (lambda () (interactive) (prog1 (god-local-mode) (keyboard-escape-quit))))
  (setq god-mode-alist '((nil . "C-") ("g" . "M-") ("G" . "C-M-")))
  (setq god-mode-enable-function-key-translation nil)
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil))

(use-package evil-god-state)

(use-package diminish)

(use-package evil
  :after (god-mode evil-god-state diminish)
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
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-define-key
    'normal global-map ","
    'evil-execute-in-god-state)
  (add-hook 'evil-god-state-entry-hook
            (lambda () (diminish 'god-local-mode)))
  (add-hook 'evil-god-state-exit-hook
            (lambda () (diminish-undo 'god-local-mode)))
  (evil-define-key
    'god global-map [escape]
    'evil-god-state-bail))

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

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

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
    :config (doom-modeline-mode 1)
    :custom (
             (doom-modeline-height 15)
             (doom-modeline-enable-word-count t)
             (doom-modeline-continuous-word-count-modes
              '(markdown-mode gfm-mode org-mode text-mode)))))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-enable-god-mode-support))

(use-package flx)

(when (not (version< emacs-version "27.1"))
  (use-package marginalia
                                        ; Either bind `marginalia-cycle` globally or only in the minibuffer
    :bind (("M-A" . marginalia-cycle)
           :map minibuffer-local-map
           ("M-A" . marginalia-cycle))

                                        ; The :init configuration is always executed (Not lazy!)
    :init
                                        ; Must be in the :init section of use-package such that the mode gets
                                        ; enabled right away. Note that this forces loading the package.
    (marginalia-mode)))

(when (not (version< emacs-version "26.1"))
  (use-package embark
    :ensure t
    :bind
    (
     ("C-c C-." . embark-act)         ; pick some comfortable binding
     ("C-;" . embark-dwim)        ; good alternative: M-.
     ("C-h B" . embark-bindings) ; alternative for `describe-bindings'
     :map embark-file-map
     ("t" . find-file-other-tab))
    :init
                                        ; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)
    :config
    (add-to-list 'marginalia-prompt-categories '("tab by name" . tab))
                                        ; Hide the mode line of the Embark live/completions buffers
    (require 'embark)
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))
    (keymap-set
     embark-file-map
     "t" #'(lambda ()
             (interactive)
             (call-interactively #'find-file-other-tab)))))

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
                ("C-<return>" . vertico-exit-input)
                ("DEL" . vertico-directory-delete-char)
                ("C-h" . vertico-directory-delete-word))
    :init
    (vertico-mode)
    (vertico-multiform-mode)))

(when (not (version< emacs-version "26.1"))
  (use-package orderless
    :demand t
    :init
                                        ; Configure a custom style dispatcher (see the Consult wiki)
                                        ; (setq orderless-style-dispatchers '(+orderless-dispatch)
                                        ;       orderless-component-separator #'orderless-escapable-split-on-space)
    (setq completion-styles '(basic substring partial-completion orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion))))
    :config
    (setq orderless-matching-styles '(orderless-flex))))

(when (not (version< emacs-version "26.3"))
  (use-package consult
    :after (vertico perspective)
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

    :hook (completion-list-mode . consult-preview-at-point-mode)

                                        ; The :init configuration is always executed (Not lazy)
    :init
                                        ; preview for `consult-register', `consult-register-load',
                                        ; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

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
                                        ; (setq consult-preview-key (list (kbd "<S-down>") (kbd
                                        ;"<S-up>")))
                                        ; For some commands and buffer sources it is useful to
                                        ; configure the
                                        ; :preview-key on a per-command basis using the
                                        ;`consult-customize' macro.
    (consult-customize
     consult-theme
     :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-recent-file
     consult--source-project-recent-file)

                                        ; Optionally configure the narrowing key.
                                        ; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ; (kbd "C-+")

                                        ; Optionally make narrowing help available in the minibuffer.
                                        ; You may want to use `embark-prefix-help-command' or which-key
                                        ;instead.
                                        ; (define-key consult-narrow-map (vconcat consult-narrow-key
                                        ;"?") #'consult-narrow-help)

                                        ; By default `consult-project-function' uses `project-root'
                                        ;from project.el.
                                        ; Optionally configure a different project root function.
                                        ; There are multiple reasonable alternatives to chose from.
                                        ; 1. project.el (the default)
                                        ; (setq consult-project-function
                                        ;#'consult--default-project--function)
                                        ; 2. projectile.el (projectile-project-root)
                                        ; (autoload 'projectile-project-root "projectile")
                                        ; (setq consult-project-function (lambda (_)
                                        ; (projectile-project-root)))
                                        ; 3. vc.el (vc-root-dir)
                                        ; (setq consult-project-function (lambda (_) (vc-root-dir)))
                                        ; 4. locate-dominating-file
                                        ; (setq consult-project-function (lambda (_)
                                        ; (locate-dominating-file "." ".git")))
    ))

(defun consult-grep-current-dir ()
  "Call `consult-grep' for the current buffer (a single file)."
  (interactive)
  (let ((consult-project-function (lambda (x) "./")))
    (consult-grep)))

(defun consult-find-current-dir ()
  "Call `consult-find' for the current buffer (a single file)."
  (interactive)
  (let ((consult-project-function (lambda (x) "./")))
    (consult-find)))

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

(defhydra hydra-jump-files (:exit t)
  "jump to files"
  ("a" (find-file
        (expand-file-name (concat phd-thesis-org-files-dir "/main.org")))
   "Agenda")
  ("t" (find-file
        (expand-file-name (concat phd-thesis-org-files-dir "/todo.org")))
   "Todos")
  ("e" (find-file
        (expand-file-name "config.org" user-emacs-directory))
   "Emacs config")
  ("w" (find-file
        (expand-file-name
         (concat seminar-dir "/Reports/2023/monogenic_certificates_compact_case/main.tex")))
   "Current report")
  ("rp" (find-file
         (expand-file-name (concat phd-thesis-write-ups-dir "/references.bib")))
   "Bibtex references - PhD thesis")
  ("rs" (find-file
         (expand-file-name (concat scc-reports-dir "/references.bib")))
   "Bibtex references - SCC project"))

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("c" change-font-size "change font size" :exit t)
  ("q" nil "finished" :exit t))

(efs/leader-keys
  "tf" '(hydra-text-scale/body :which-key "change (f)ont size"))

(use-package zoom
  :config
  (setq zoom-size '(0.618 . 0.618)))

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
    (set-face-attribute (car face) nil :font "Hack" :weight 'regular :height (cdr face)))

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
          ("\\.pdf\\'" . "sioyek %s")
          ("\\.nb?\\'" . "Mathematica %s")))

  (setq org-ellipsis "⇓")
  (setq org-hierarchical-todo-statistics nil)

  (setq
   org-agenda-files
   (mapcar
    #'(lambda (x) (concat phd-thesis-org-files-dir "/" x))
    '(
      "20231115200616-qm_seminar.org"
      "research_tasks.org"
      "school_tasks.org"
      "graduation_logistics.org"
      "dissertation_tasks.org"
      "main.org"
      "todo.org"
      )))

  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-window-setup 'current-window)
  (setq org-indirect-buffer-display 'current-window)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (require 'org-habit)
  (require 'org-protocol)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "EXTERNAL" "|")
          (sequence "GOAL" "IDEA" "OBSERVATION" "|" "OK")
          (sequence "TODO" "|" "MOVED" "COMPLETED(c)" "CANC(k@)")
          (sequence "EMAIL" "|")))

                                        ; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

                                        ; Use find-file instead of file-find-other-window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  (setq org-tag-alist
        '((:startgroup)
          ("seminar" . ?s)
          ("thesis" . ?t)
          ("graduation" . ?g)
          (:endgroup)
          ("review" . ?r)
          ("interesting" . ?i)
          ("now" . ?n)))

  (setq org-capture-templates
        `(
          ("e" "Email Capture")
          ("ea" "Main Agenda" entry
           (file+olp agenda-mail "EMAIL")
           "** TODO Check this email %a"
           :immediate-finish t)
          ("ed" "Dissertation Tasks" entry
           (file+olp dissertation-tasks-mail "EMAIL")
           "** TODO Check this email %a"
           :immediate-finish t)
          ("eg" "Graduation Logistics" entry
           (file+olp graduation-logistics-tasks-mail "EMAIL")
           "** TODO Check this email %a"
           :immediate-finish t)
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
          ("et" "TA Task")
          ("etu" "CS 105 - Fall 2023" entry
           (file+olp ta1-tasks-mail "EMAIL")
           "** TODO Check this email %a"
           :immediate-finish t)
          ("eto" "CS 561 - Fall 2023" entry
           (file+olp ta2-tasks-mail "EMAIL")
           "** TODO Check this email %a"
           :immediate-finish t)
          ("em" "MaxDiff Agenda" entry
           (file+olp maxdiff-agenda-mail "EMAIL")
           "** TODO Check this email %a"
           :immediate-finish t)
          ))

  (define-key org-mode-map (kbd "C-c d")
              (lambda () (interactive) (org-todo "MOVED")))
  (define-key org-mode-map (kbd "C-c c")
              (lambda () (interactive) (org-todo "COMPLETED")))
  (define-key org-mode-map (kbd "C-c t")
              (lambda () (interactive) (org-todo "TODO")))
  (define-key org-mode-map (kbd "C-c k")
              (lambda () (interactive) (org-todo "CANC")))
  (define-key org-mode-map (kbd "C-c i")
              (lambda () (interactive) (org-todo "IDEA")))
  (define-key org-mode-map (kbd "C-c o")
              (lambda () (interactive) (org-todo "OK")))
  (define-key org-mode-map (kbd "C-c <return>")
              'org-insert-heading-respect-content)
  (define-key org-mode-map (kbd "C-c C-<SPC>")
              'org-insert-subheading)
  (define-key org-mode-map (kbd "C-c C-<return>")
              'org-meta-return)
  (define-key org-mode-map (kbd "C-c s")
              (lambda () (interactive) (org-sort-buffer)))

  (efs/org-font-setup))

(efs/leader-keys
  "o" '(:ignore t :which-key "(o)rg")
  "oc" '(org-capture nil :which-key "org-(c)apture"))

(use-package org-mime
  :ensure t)

(defun org-sort-buffer ()
  "Sort all entries in the current buffer, recursively."
  (interactive)
  (mark-whole-buffer)
  (org-sort-entries nil ?o)
  (org-map-entries (lambda ()
                     (condition-case x
                         (org-sort-entries nil ?o)
                       (user-error)))))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

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

(use-package org-download)

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/GithubProjects/phd-thesis/Documents/Org-Files")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
      :unnarrowed t)
     ("l" "lecture" plain
      (file "~/Documents/GithubProjects/phd-thesis/Documents/Org-Files/Templates/lecture.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+DATE: %U\n")
      :unnarrowed t)
     ("m" "meeting" plain
      (file "~/Documents/GithubProjects/phd-thesis/Documents/Org-Files/Templates/meeting.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+DATE: %U\n")
      :unnarrowed t)))
  :bind (("C-x n f" . org-roam-node-find)
         ("C-x n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         ("C-<return>" . vertico-exit-input))
  :config
  (org-roam-setup))

(when (not (version< emacs-version "26.3"))
  (use-package ox-hugo
    :ensure t
    :pin melpa
    :after ox))

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
      See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: "))
           (curdate (format-time-string "%Y-%m-%d"))
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ,(concat ":EXPORT_DATE: " curdate)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h" "Hugo post" entry
                 (file+olp website-posts "Posts")
                 (function org-hugo-new-subtree-post-capture-template)
                 :prepend t)))

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
  (olivetti-mode 1)
  (visual-line-mode 1))

(use-package olivetti
  :hook ((org-mode . efs/org-mode-visual-fill)
         (markdown-mode . efs/org-mode-visual-fill)
         (TeX-mode . efs/org-mode-visual-fill)
         (LaTeX-mode . efs/org-mode-visual-fill)
         (mu4e-main-mode . efs/org-mode-visual-fill))
  :custom
  (olivetti-style 'fancy)
  (olivetti-margin-width 5)
  (olivetti-body-width 85))

(custom-set-faces
 '(olivetti-fringe ((t :background "#242424"))))

;; (custom-set-faces
;;  '(olivetti-fringe ((t :background unspecified))))
(defun custom-olivetti-mode-on-hook ()
  (setq-local flycheck-indication-mode 'left-margin))

(defun custom-olivetti-mode-off-hook ()
  (kill-local-variable 'flycheck-indication-mode))

(add-hook 'olivetti-mode-on-hook 'custom-olivetti-mode-on-hook)
(add-hook 'olivetti-mode-off-hook 'custom-olivetti-mode-off-hook)

(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
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

(defun restart-yasnippet ()
  (interactive)
  (add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets))

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
    (setq lsp-completion-provider :none)
    (defun corfu-lsp-setup ()
      (setq-local completion-styles '(orderless)
                  completion-category-defaults nil))
    (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
    (lsp-enable-which-key-integration t)))

(when (not (version< emacs-version "26.1"))
  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom)))

(when (fboundp 'module-load)
  (use-package tree-sitter
    :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
    :hook ((latex-mode python-mode rustic-mode) . tree-sitter-hl-mode)
    :init
    (setq treesit-language-source-alist
          '((latex . ("https://github.com/latex-lsp/tree-sitter-latex"))))
    :config
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(rustic-mode . rust))
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(TeX-mode . latex))
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(LaTeX-mode . latex))
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(latex-mode . latex))
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(bibtex-mode . bibtex))
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(org-mode . org))
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(c-mode . c))
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(cpp-mode . cpp))
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(python-mode . python))
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(typescript-mode . typescript))
    (defun nf/treesit-install-all-languages ()
      "Install all languages specified by `treesit-language-source-alist'."
      (interactive)
      (let ((languages (mapcar 'car treesit-language-source-alist)))
        (dolist (lang languages)
          (treesit-install-language-grammar lang)
          (message "`%s' parser was installed." lang)

          (sit-for 0.75))))))

(when (fboundp 'module-load)
  (use-package tree-sitter-langs
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

; (when (not (version< emacs-version "26.1"))
;   (use-package dap-mode
;                                         ; :custom
;                                         ; (lsp-enable-dap-auto-configure nil)
;                                         ; :config
;                                         ; (dap-ui-mode 1)
;     :commands dap-debug
;     :config
;                                         ; Set up Node debugging
;     (require 'dap-node)
;     (dap-node-setup) ; Automatically installs Node debug adapter if needed

;                                         ; Bind `C-c l d` to `dap-hydra` for easy access
;     (general-define-key
;      :keymaps 'lsp-mode-map
;      :prefix lsp-keymap-prefix
;      "d" '(dap-hydra t :wk "debugger"))))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package rustic)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(add-hook 'TeX-mode-hook 'outline-minor-mode)
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

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
    (setq lsp-latex-forward-search-executable
          "/Applications/Skim.app/Contents/SharedSupport/displayline")
    (setq lsp-latex-forward-search-args '("%l" "%p" "%f"))))
;; (setq lsp-latex-forward-search-executable "/opt/homebrew/bin/sioyek")
;;     (setq lsp-latex-forward-search-args
;;           '( 
;;              "--forward-search-file"
;;              "%f"
;;              "--forward-search-line"
;;              "%l"
;;              "%p"))))

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
    (setq reftex-plug-into-AUCTeX t)
    (setq reftex-insert-label-flags (list t nil))
    (setq reftex-ref-macro-prompt nil)
    (setq font-latex-fontify-script nil)))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

                                        ;(use-package outline-magic)

                                        ;(evil-define-key 'normal
                                        ;  outline-minor-mode-map (kbd "<tab>") 'outline-toggle-children)
                                        ;(evil-define-key 'normal
                                        ;  outline-minor-mode-map (kbd "<S-tab>") 'outline-cycle)

(efs/leader-keys
  "l" '(:ignore t :which-key "(l)atex related")
  "lr" '((lambda () (interactive)
           (reftex-view-crossref))
         :which-key "Goto xref (r)erence")
  "lp" '((lambda () (interactive)
           (yasnippet/goto-parent-file))
         :which-key "Goto (p)arent")
  "lF" '((lambda () (interactive)
           (LaTeX-fill-buffer nil))
         :which-key "Latex (F)ill buffer")
  "lf" '((lambda () (interactive)
           (lsp-latex-forward-search))
         :which-key "Latex (f)orward search"))

(use-package consult-reftex
  :after consult
  :straight
  (:type git
         :host github
         :repo "karthink/consult-reftex"))

(when (not (version< emacs-version "27.1"))
  (use-package citar
    :bind (("C-c b" . citar-insert-citation)
           :map minibuffer-local-map
           ("M-b" . citar-insert-preset))
    :custom
    (citar-bibliography `(,(concat scc-reports-dir "/references.bib")
                          ,(concat maxdiff-write-ups-dir "/references.bib")
                          ,(concat phd-thesis-write-ups-dir "/references.bib")))))

(use-package typst-mode
  :straight
  (:type git
         :host github
         :repo "Ziqi-Yang/typst-mode.el"))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
                                        ; (dap-python-executable "python3")
                                        ; (dap-python-debugger 'debugpy)
  :config
                                        ; (require 'dap-python)
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

(use-package boogie-friends
  :config
  (setq
   flycheck-z3-executable
   "~/Documents/GithubProjects/CAXDInterpolator/dependencies/z3-interp-plus/build/z3"))

(use-package lean4-mode
  :straight (lean4-mode :type git
                        :host github
                        :repo "leanprover/lean4-mode")
  :commands (lean4-mode))

(setq scheme-program-name "racket")
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
                                        ; Choose indentation mode (the latter requires haskell-mode >= 2.5):
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
                                        ; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  )
(use-package lsp-haskell)

(use-package parinfer
  :disabled
  :hook ((clojure-mode . parinfer-mode)
         (emacs-lisp-mode . parinfer-mode)
         (common-lisp-mode . parinfer-mode)
         (scheme-mode . parinfer-mode)
         (lisp-mode . parinfer-mode))
  :config
  (setq parinfer-extensions
        '(defaults       ; should be included.
          pretty-parens  ; different paren styles for different modes.
          evil           ; If you use Evil.
          smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
          smart-yank)))  ; Yank behavior depend on mode.

(efs/leader-keys
  "tp" 'parinfer-toggle-mode)

(use-package corfu
  :after orderless
                                        ; Optional customizations
  :general
  (:keymaps 'corfu-map
            :states 'insert
            "C-n" #'corfu-next
            "C-p" #'corfu-previous
            "<return>" #'corfu-insert
            "M-d" #'corfu-show-documentation
            "M-l" #'corfu-show-location)
  :custom
  (corfu-cycle t)                ; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ; Enable auto completion
  (corfu-separator ?\s)          ; Orderless field separator
                                        ; (corfu-quit-at-boundary nil)
                                        ; Never quit at completion boundary
                                        ; (corfu-quit-no-match nil)
                                        ; Never quit, even if there is no match
                                        ; (corfu-preview-current nil)
                                        ; Disable current candidate preview
                                        ; (corfu-preselect 'prompt)
                                        ; Preselect the prompt
                                        ; (corfu-on-exact-match nil)
                                        ; Configure handling of exact matches
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-scroll-margin 5)        ; Use scroll margin
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
                                        ; (completion-styles '(basic))

                                        ; Enable Corfu only for certain modes.
                                        ; :hook ((prog-mode . corfu-mode)
                                        ;        (shell-mode . corfu-mode)
                                        ;        (eshell-mode . corfu-mode))

                                        ; Recommended: Enable Corfu globally.
                                        ; This is recommended since Dabbrev can be used
                                        ; globally (M-/).
                                        ; See also `global-corfu-modes'.
  :config
  (setq corfu-popupinfo-delay 0.2)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons nil)
                                        ; Have background color be the same as `corfu' face
                                        ; background
  (kind-icon-default-face 'corfu-default)
                                        ; Use midpoint color between foreground and background
                                        ; colors
                                        ; ("blended")?
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package cape
                                        ; Bind dedicated completion commands
                                        ; Alternative prefix keys: C-c p, M-p, M-+, ...
                                        ;:bind (("C-c p p" . completion-at-point) ; capf
                                        ;       ("C-c p t" . complete-tag)        ; etags
                                        ;       ("C-c p d" . cape-dabbrev)        ; or
                                        ; dabbrev-completion
                                        ;       ("C-c p h" . cape-history)
                                        ;       ("C-c p f" . cape-file)
                                        ;       ("C-c p k" . cape-keyword)
                                        ;       ("C-c p s" . cape-elisp-symbol)
                                        ;       ("C-c p e" . cape-elisp-block)
                                        ;       ("C-c p a" . cape-abbrev)
                                        ;       ("C-c p l" . cape-line)
                                        ;       ("C-c p w" . cape-dict)
                                        ;       ("C-c p :" . cape-emoji)
                                        ;       ("C-c p \\" . cape-tex)
                                        ;       ("C-c p _" . cape-tex)
                                        ;       ("C-c p ^" . cape-tex)
                                        ;       ("C-c p &" . cape-sgml)
                                        ;       ("C-c p r" . cape-rfc1345))
  :init
                                        ; Add to the global default value of
                                        ; `completion-at-point-functions' which is
                                        ; used by `completion-at-point'.  The order of the functions
                                        ; matters, the
                                        ; first function returning a result wins.  Note that the list
                                        ; of buffer-local
                                        ; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
                                        ; (add-to-list 'completion-at-point-functions
                                        ; #'cape-elisp-block)
                                        ; (add-to-list 'completion-at-point-functions #'cape-history)
                                        ; (add-to-list 'completion-at-point-functions #'cape-keyword)
                                        ; (add-to-list 'completion-at-point-functions #'cape-tex)
                                        ; (add-to-list 'completion-at-point-functions #'cape-sgml)
                                        ; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
                                        ; (add-to-list 'completion-at-point-functions #'cape-abbrev)
                                        ; (add-to-list 'completion-at-point-functions #'cape-dict)
                                        ; (add-to-list 'completion-at-point-functions
                                        ; #'cape-elisp-symbol)
                                        ; (add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  (setq yasnippet-capf-lookup-by 'name))

(when (not (version< emacs-version "26.3"))
  (use-package magit
    :commands magit-status
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)))

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

(use-package fzf
  :bind
                                        ; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
                                        ; command used for `fzf-grep-*` functions
                                        ; example usage for ripgrep:
                                        ; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
                                        ; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(use-package gptel
  :straight
  (:type git
         :host github
         :repo "karthink/gptel")
  :config
  (setq gptel-api-key
        (let ((key (shell-command-to-string "pass personal/chatgpt")))
          (string-trim key))))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh")
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
  (setq dired-guess-shell-alist-user '(("\\.nb?\\'" "Mathematica")
                                       ("\\.pdf\\'" "sioyek")))
  (when (not (version< emacs-version "26.3"))
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer)))

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

(use-package dired-single
  :commands (dired dired-jump))

(when (not (version< emacs-version "26.1"))
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode)))

(use-package dired-open
  :commands (dired dired-jump)
  :config
                                        ; Doesn't work as expected!
                                        ; (add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(defun linkify (msg)
  "Returns an org-link"
  (interactive "sDescription: ")
  (insert
   (concat "[[file:"
           (abbreviate-file-name (buffer-file-name))
           "]["
           (if (equal msg "") (buffer-name) msg)
           "]]")))

(defun snakify (input)
  (replace-regexp-in-string
   " "
   "_"
   (downcase input)))

(defun insert-snake ()
  (interactive)
  (insert (snakify (car kill-ring))))

(defun reftexify ()
  (interactive)
  (shell-command "~/.local/scripts/reftexify"))

(defun quicktex (name type)
  (interactive
   (list
    (read-string "Project name: ")
    (read-string "Project type: ")))
  (shell-command (concat "~/.local/bin/quicktex " name " " type)))

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

(efs/leader-keys
  "p" '(:ignore t :which-key "(p)resentation")
  "pp" '(org-tree-slide-move-previous-tree :which-key "Previous slide")
  "pn" '(org-tree-slide-move-next-tree  :which-key "Next slide"))

(use-package simpleclip
  :config
  (simpleclip-mode 1))

(use-package markdown-preview-eww)

(use-package sqlite3)

(use-package jinx
  :init
  (setenv "PKG_CONFIG_PATH"
          (concat "/opt/homebrew/opt/glib/lib/pkgconfig/:"
                  (getenv "PKG_CONFIG_PATH"))) 
  :config
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))
  (vertico-multiform-mode 1))

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . poly-markdown+r-mode)
          ("overleaf\\.com" . latex-mode))))

(use-package try)

(use-package zoxide)
