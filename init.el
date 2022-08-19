#+title: Emacs Configuration
#+PROPERTY: header-args:emacs-lisp :tangle ./init.el :mkdirp yes

* Tips for Use

In this document I've added links in many places that lead you to documentation for the various packages we use.  If you're looking at this file in Emacs, you can put your cursor on a link and press =C-c C-o= or run =M-x org-open-at-point= to open the link in your web browser.

* Package System Setup

** Straight.el setup

#+begin_src emacs-lisp

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

#+end_src 

Emacs has a built in package manager but it doesn't make it easy to automatically install packages on a new system the first time you pull down your configuration.  [[https://github.com/jwiegley/use-package][use-package]] is a really helpful package used in this configuration to make it a lot easier to automate the installation and configuration of everything else we use.

#+begin_src emacs-lisp

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

#+end_src

** Automatic Package Updates

The auto-package-update package helps us keep our Emacs packages up to date!  It will prompt you after a certain number of days either at startup or at a specific time of day to remind you to update your packages.

You can also use =M-x auto-package-update-now= to update right now!

#+begin_src emacs-lisp

  (use-package auto-package-update
    :custom
    (auto-package-update-hide-results t)
    (auto-package-update-delete-old-versions t))

#+end_src

* Startup Performance

#+begin_src emacs-lisp

                                          ; The default is 800 kilobytes.  Measured in bytes.
  (setq gc-cons-threshold (* 50 1000 1000))

  (defun efs/display-startup-time ()
    (message "Emacs loaded in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done))

  (add-hook 'emacs-startup-hook #'efs/display-startup-time)

#+end_src

* Settings

#+begin_src emacs-lisp

                                          ; NOTE: init.el is now generated from config.org.  Please edit that file
                                          ;       in Emacs and init.el will be generated automatically!

                                          ; You will most likely need to adjust this font size for your system!
  (defvar efs/default-font-size 160)
  (defvar efs/default-variable-font-size 160)

                                          ; Make frame transparency overridable
  (defvar efs/frame-transparency '(90 . 90))

  (defvar phd-thesis-dir "~/Documents/GithubProjects/phd-thesis")
  (defvar ta-org-files-dir 
    (concat phd-thesis-dir
            "/Documents/Semesters/Fall/2022/TA-CS-241/Org-Files"))
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

  (defvar seminar-dir (concat phd-thesis-dir "/Documents/Seminars/BeihangUniversity-Fall2021"))
  (defvar seminar-org-files-dir (concat seminar-dir "/Org-Files"))
  (defvar ta-tasks-mail 
    (concat ta-org-files-dir "/current_tasks.org"))

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

#+end_src

* Basic UI Configuration

This section configures basic UI settings that remove unneeded elements to make Emacs look a lot more minimal and modern.  If you're just getting started in Emacs, the menu bar might be helpful so you can remove the =(menu-bar-mode -1)= line if you'd like to still see that.

#+begin_src emacs-lisp

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

                                          ; Set up the visible bell
  (setq visible-bell t)

  (column-number-mode)
  (setq-default display-line-numbers-type 'visual) 

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

#+end_src

** Font Configuration

I am using the [[https://github.com/tonsky/FiraCode][Fira Code]] and [[https://fonts.google.com/specimen/Cantarell][Cantarell]] fonts for this configuration which will more than likely need to be installed on your machine.  Both can usually be found in the various Linux distro package managers or downloaded from the links above.

#+begin_src emacs-lisp

  (set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

                                          ; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

                                          ; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Fira Code Retina" :height efs/default-variable-font-size :weight 'regular)

#+end_src

** Keybinding Configuration

This configuration uses [[https://evil.readthedocs.io/en/latest/index.html][evil-mode]] for a Vi-like modal editing experience.  [[https://github.com/noctuid/general.el][general.el]] is used for easy keybinding configuration that integrates well with which-key.  [[https://github.com/emacs-evil/evil-collection][evil-collection]] is used to automatically configure various Emacs modes with Vi-like keybindings for evil-mode.

#+begin_src emacs-lisp

                                          ; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-set-key (kbd "C-i") 'evil-jump-forward)
  (global-set-key (kbd "C-o") 'evil-jump-backward)
  (global-set-key [(control x) (k)] 'kill-buffer)

  (use-package general
    :after evil
    :config
    (general-create-definer efs/leader-keys
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC")

    (efs/leader-keys 
      "e" '(:ignore t :which-key "(e)dit buffer")
      "ec"  '(evilnc-comment-or-uncomment-lines :which-key "(c)omment line")
      "ei"  '((lambda () (interactive) (indent-region (point-min) (point-max))) :which-key "(i)ndent buffer")
      "ey" '(simpleclip-copy :which-key "clipboard (y)ank")
      "ep" '(simpleclip-paste :which-key "clipboard (p)aste")
      "f" '(:ignore t :which-key "edit (f)iles")
      "fa" '((lambda () (interactive) (find-file (expand-file-name (concat phd-thesis-org-files-dir "/main.org")))) :which-key "(a)genda")
      "fe" '((lambda () (interactive) (find-file (expand-file-name "config.org" user-emacs-directory))) :which-key "(e)macs source")
      "fw" '((lambda () (interactive) (find-file (expand-file-name (concat seminar-dir "/Reports/finding_certificates_qm_univariate/main.tex")))) :which-key "Current (w)ork")
      "fr" '(:ignore t :which-key "Edit (r)eferences")
      "frp" '((lambda () (interactive) (find-file (expand-file-name (concat phd-thesis-write-ups-dir "/references.bib")))) :which-key "Edit (p)hD references")
      "frs" '((lambda () (interactive) (find-file (expand-file-name (concat scc-reports-dir "/references.bib")))) :which-key "Edit (s)CC references")
      "s"  '(shell-command :which-key "(s)hell command")
      "t"  '(:ignore t :which-key "(t)oggles")
      "tt" '(counsel-load-theme :which-key "Choose (t)heme")
      "g" '(magit-status :which-key "Ma(g)it status")
      "d" '(dired-jump :which-key "(d)ired jump")
      "m" '(mu4e :which-key "(m)u4e")
      "l" '(:ignore t :which-key "(l)atex related")
      "lp" '((lambda () (interactive) (yasnippet/goto-parent-file)) :which-key "Goto (p)arent")
      "lf" '((lambda () (interactive) (LaTeX-fill-buffer nil)) :which-key "Latex (f)ill buffer")
      "lF" '((lambda () (interactive) (lsp-latex-forward-search)) :which-key "Latex (f)orward search")
      "o" '(org-capture nil :which-key "(o)rg-capture")
      "w" '(:ignore t :which-key "(w)indows related")
      "wu" '(winner-undo :which-key "Winner (u)ndo")
      "wr" '(winner-redo :which-key "Winner (r)edo")))

  (use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil)
    :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

                                          ; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))

#+end_src

* UI Configuration

** Command Log Mode

[[https://github.com/lewang/command-log-mode][command-log-mode]] is useful for displaying a panel showing each key binding you use in a panel on the right side of the frame.  Great for live streams and screencasts!

#+begin_src emacs-lisp

  (use-package command-log-mode
    :commands command-log-mode)

#+end_src

** Color Theme

[[https://github.com/hlissner/emacs-doom-themes][doom-themes]] is a great set of themes with a lot of variety and support for many different Emacs modes.  Taking a look at the [[https://github.com/hlissner/emacs-doom-themes/tree/screenshots][screenshots]] might help you decide which one you like best.  You can also run =M-x counsel-load-theme= to choose between them easily.

#+begin_src emacs-lisp

  (use-package doom-themes
    :init (load-theme 'doom-gruvbox t))

#+end_src

*** Better Modeline

[[https://github.com/seagle0128/doom-modeline][doom-modeline]] is a very attractive and rich (yet still minimal) mode line configuration for Emacs.  The default configuration is quite good but you can check out the [[https://github.com/seagle0128/doom-modeline#customize][configuration options]] for more things you can enable or disable.

*NOTE:* The first time you load your configuration on a new machine, you'll need to run `M-x all-the-icons-install-fonts` so that mode line icons display correctly.

#+begin_src emacs-lisp

  (use-package all-the-icons)

  (use-package anzu)

  (use-package evil-anzu
    :config (global-anzu-mode 1)
    (setq anzu-minimum-input-length 4))

#+end_src

** Which Key

[[https://github.com/justbur/emacs-which-key][which-key]] is a useful UI panel that appears when you start pressing any key binding in Emacs to offer you all possible completions for the prefix.  For example, if you press =C-c= (hold control and press the letter =c=), a panel will appear at the bottom of the frame displaying all of the bindings under that prefix and which command they run.  This is very useful for learning the possible key bindings in the mode of your current buffer.

#+begin_src emacs-lisp

  (use-package which-key
    :defer 0
    :diminish which-key-mode
    :config
    (which-key-mode)
    (setq which-key-idle-delay 1))

#+end_src

** Ivy and Counsel

[[https://oremacs.com/swiper/][Ivy]] is an excellent completion framework for Emacs.  It provides a minimal yet powerful selection menu that appears when you open files, switch buffers, and for many other tasks in Emacs.  Counsel is a customized set of commands to replace `find-file` with `counsel-find-file`, etc which provide useful commands for each of the default completion commands.

[[https://github.com/Yevgnen/ivy-rich][ivy-rich]] adds extra columns to a few of the Counsel commands to provide more information about each item.

#+begin_src emacs-lisp

  (use-package ivy
    :diminish
    :bind (("C-s" . swiper)
           :map ivy-minibuffer-map
           ("TAB" . ivy-alt-done)
           ("C-l" . ivy-alt-done)
           ("C-j" . ivy-next-line)
           ("C-k" . ivy-previous-line)
           :map ivy-switch-buffer-map
           ("C-k" . ivy-previous-line)
           ("C-l" . ivy-done)
           ("C-d" . ivy-switch-buffer-kill)
           :map ivy-reverse-i-search-map
           ("C-k" . ivy-previous-line)
           ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))

  (use-package ivy-rich
    :after ivy
    :init
    (ivy-rich-mode 1))

  (use-package flx)

  (setq ivy-initial-inputs-alist nil)

  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))

  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))

  (use-package counsel
    :bind (("C-M-j" . 'counsel-switch-buffer)
           :map minibuffer-local-map
           ("C-r" . 'counsel-minibuffer-history))
    :custom
    (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
    :config
    (counsel-mode 1))

#+end_src

*** Improved Candidate Sorting with prescient.el

prescient.el provides some helpful behavior for sorting Ivy completion candidates based on how recently or frequently you select them.  This can be especially helpful when using =M-x= to run commands that you don't have bound to a key but still need to access occasionally.

This Prescient configuration is optimized for use in System Crafters videos and streams, check out the [[https://youtu.be/T9kygXveEz0][video on prescient.el]] for more details on how to configure it!

#+begin_src emacs-lisp

  (use-package ivy-prescient
    :after counsel
    :custom
    (ivy-prescient-enable-filtering nil)
    :config
                                          ; Uncomment the following line to have sorting remembered across sessions!
                                          ;  (prescient-persist-mode 1)
    (ivy-prescient-mode 1))

#+end_src

** Selectrum

#+begin_src emacs-lisp 

  (use-package selectrum
    :straight t
    :config
    (selectrum-mode +1))

  (use-package selectrum-prescient
    :straight t
    :after selectrum
    :config
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1))

#+end_src

** Consult

#+begin_src emacs-lisp 
  
  (use-package consult
    :after selectrum
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
           ("C-c C-l" . consult-line)
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
    )

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

#+end_src

** Helpful Help Commands

[[https://github.com/Wilfred/helpful][Helpful]] adds a lot of very helpful (get it?) information to Emacs' =describe-= command buffers.  For example, if you use =describe-function=, you will not only get the documentation about the function, you will also see the source code of the function and where it gets used in other places in the Emacs configuration.  It is very useful for figuring out how things work in Emacs.

#+begin_src emacs-lisp

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

#+end_src

** Text Scaling

This is an example of using [[https://github.com/abo-abo/hydra][Hydra]] to design a transient key binding for quickly adjusting the scale of the text on screen.  We define a hydra that is bound to =C-s t s= and, once activated, =j= and =k= increase and decrease the text scale.  You can press any other key (or =f= specifically) to exit the transient key map.

#+begin_src emacs-lisp

  (use-package hydra
    :defer t)

  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("k" text-scale-increase "in")
    ("j" text-scale-decrease "out")
    ("q" nil "finished" :exit t))

  (efs/leader-keys
    "ts" '(hydra-text-scale/body :which-key "scale text"))

#+end_src

* Org Mode

[[https://orgmode.org/][Org Mode]] is one of the hallmark features of Emacs.  It is a rich document editor, project planner, task and time tracker, blogging engine, and literate coding utility all wrapped up in one package.

** Better Font Faces

The =efs/org-font-setup= function configures various text faces to tweak the sizes of headings and use variable width fonts in most cases so that it looks more like we're editing a document in =org-mode=.  We switch back to fixed width (monospace) fonts for code blocks and tables so that they display correctly.

#+begin_src emacs-lisp

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
      (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'regular :height (cdr face)))

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
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

#+end_src

** Configure Babel Languages

To execute or export code in =org-mode= code blocks, you'll need to set up =org-babel-load-languages= for each language you'd like to use.  [[https://orgmode.org/worg/org-contrib/babel/languages.html][This page]] documents all of the languages that you can use with =org-babel=.

#+begin_src emacs-lisp

  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)))

    (push '("conf-unix" . conf-unix) org-src-lang-modes))

#+end_src

This section contains the basic configuration for =org-mode= plus the configuration for Org agendas and capture templates.  There's a lot to unpack in here so I'd recommend watching the videos for [[https://youtu.be/VcgjTEa0kU4][Part 5]] and [[https://youtu.be/PNE-mgkZ6HM][Part 6]] for a full explanation.

#+begin_src emacs-lisp

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
            ("\\.pdf\\'" . "zathura %s")))

    (setq org-ellipsis " ▾")

    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)

    (require 'org-habit)
    (require 'org-protocol)
    (add-to-list 'org-modules 'org-habit)
    (setq org-habit-graph-column 60)

    (setq org-todo-keywords
          '((sequence "GOAL(g)" "REMINDER(r!)" "|")
            (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
            (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")
            (sequence "EMAIL(e)" "|")))

    (setq org-refile-targets
          '(("Archive.org" :maxlevel . 1)
            ("Tasks.org" :maxlevel . 1)))

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

                                          ; Configure custom agenda views
    (setq org-agenda-custom-commands
          '(("d" "Dashboard"
             ((agenda "" ((org-deadline-warning-days 7)))
              (todo "NEXT"
                    ((org-agenda-overriding-header "Next Tasks")))
              (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

            ("n" "Next Tasks"
             ((todo "NEXT"
                    ((org-agenda-overriding-header "Next Tasks")))))

            ("W" "Work Tasks" tags-todo "+work-email")

                                          ; Low-effort next actions
            ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
             ((org-agenda-overriding-header "Low Effort Tasks")
              (org-agenda-max-todos 20)
              (org-agenda-files org-agenda-files)))

            ("w" "Workflow Status"
             ((todo "WAIT"
                    ((org-agenda-overriding-header "Waiting on External")
                     (org-agenda-files org-agenda-files)))
              (todo "REVIEW"
                    ((org-agenda-overriding-header "In Review")
                     (org-agenda-files org-agenda-files)))
              (todo "PLAN"
                    ((org-agenda-overriding-header "In Planning")
                     (org-agenda-todo-list-sublevels nil)
                     (org-agenda-files org-agenda-files)))
              (todo "BACKLOG"
                    ((org-agenda-overriding-header "Project Backlog")
                     (org-agenda-todo-list-sublevels nil)
                     (org-agenda-files org-agenda-files)))
              (todo "READY"
                    ((org-agenda-overriding-header "Ready for Work")
                     (org-agenda-files org-agenda-files)))
              (todo "ACTIVE"
                    ((org-agenda-overriding-header "Active Projects")
                     (org-agenda-files org-agenda-files)))
              (todo "COMPLETED"
                    ((org-agenda-overriding-header "Completed Projects")
                     (org-agenda-files org-agenda-files)))
              (todo "CANC"
                    ((org-agenda-overriding-header "Cancelled Projects")
                     (org-agenda-files org-agenda-files)))))))

    (setq org-capture-templates
          `(("m" "Email Capture")
            ("mr" "Research Tasks" entry
             (file+olp research-tasks-mail "EMAIL")
             "** TODO Check this email %a"
             :immediate-finish t)
            ("ml" "Lunch Tasks" entry
             (file+olp lunch-tasks-mail "EMAIL")
             "** TODO Check this email %a"
             :immediate-finish t)
            ("ms" "SCC Project Tasks" entry
             (file+olp scc-tasks-mail "EMAIL")
             "** TODO Check this email %a"
             :immediate-finish t)
            ("mc" "School Tasks" entry
             (file+olp school-tasks-mail "EMAIL")
             "** TODO Check this email %a"
             :immediate-finish t)
            ("me" "Seminar Tasks" entry
             (file+olp seminar-tasks-mail "EMAIL")
             "** TODO Check this email %a"
             :immediate-finish t)
            ("mt" "TA Tasks" entry
             (file+olp ta-tasks-mail "EMAIL")
             "** TODO Check this email %a"
             :immediate-finish t)))

    (define-key global-map (kbd "C-c s")
      (lambda () (interactive) (mark-whole-buffer) (org-sort-entries nil ?o)))

    (define-key global-map (kbd "C-c d")
      (lambda () (interactive) (org-todo "DONE")))

    (define-key global-map (kbd "C-c c")
      (lambda () (interactive) (org-todo "COMPLETED")))

    (define-key global-map (kbd "C-c t")
      (lambda () (interactive) (org-todo "TODO")))

    (defun auto/SortTODO ()
      (when (and buffer-file-name (string-match ".*/todolist.org" (buffer-file-name)))
        (setq unread-command-events (listify-key-sequence "\C-c s"))))

    (efs/org-font-setup))

#+end_src

Update org-latex-classes

#+begin_src emacs-lisp 

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

#+end_src

*** Nicer Heading Bullets

[[https://github.com/sabof/org-bullets][org-bullets]] replaces the heading stars in =org-mode= buffers with nicer looking characters that you can control.  Another option for this is [[https://github.com/integral-dw/org-superstar-mode][org-superstar-mode]] which we may cover in a later video.

#+begin_src emacs-lisp

  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

#+end_src

*** Center Org Buffers

We use [[https://github.com/joostkremers/visual-fill-column][visual-fill-column]] to center =org-mode= buffers for a more pleasing writing experience as it centers the contents of the buffer horizontally to seem more like you are editing a document.  This is really a matter of personal preference so you can remove the block below if you don't like the behavior.

#+begin_src emacs-lisp

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

#+end_src

** Structure Templates

Org Mode's [[https://orgmode.org/manual/Structure-Templates.html][structure templates]] feature enables you to quickly insert code blocks into your Org files in combination with =org-tempo= by typing =<= followed by the template name like =el= or =py= and then press =TAB=.  For example, to insert an empty =emacs-lisp= block below, you can type =<el= and press =TAB= to expand into such a block.

You can add more =src= block templates below by copying one of the lines and changing the two strings at the end, the first to be the template name and the second to contain the name of the language [[https://orgmode.org/worg/org-contrib/babel/languages.html][as it is known by Org Babel]].

#+begin_src emacs-lisp

  (with-eval-after-load 'org
                                          ; This is needed as of Org 9.2
    (require 'org-tempo)

    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python")))

#+end_src

** Auto-tangle Configuration Files

This snippet adds a hook to =org-mode= buffers so that =efs/org-babel-tangle-config= gets executed each time such a buffer gets saved.  This function checks to see if the file being saved is the config.org file you're looking at right now, and if so, automatically exports the configuration here to the associated output files.

#+begin_src emacs-lisp

                                          ; Automatically tangle our config.org config file when we save it
  (defun efs/org-babel-tangle-config ()
    (when (string-equal (file-name-directory (buffer-file-name))
                        (expand-file-name user-emacs-directory))
                                          ; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

#+end_src

* Development

** Yasnippet setup

#+begin_src emacs-lisp

  (use-package yasnippet
    :config
    (setq yas-snippet-dirs `(,(expand-file-name "snippets" user-emacs-directory)))
    (setq yas-key-syntaxes '(yas-longest-key-from-whitespace "w_.()" "w_." "w_" "w"))
    (define-key yas-minor-mode-map (kbd "C-g") 'evil-normal-state)
    (define-key yas-keymap (kbd "C-g") 'evil-normal-state)
    (yas-global-mode 1))

  (use-package yasnippet-snippets)

  (load (expand-file-name "snippets/yasnippet-scripts.el" user-emacs-directory))

#+end_src

** Perspective

#+begin_src emacs-lisp

  (use-package perspective
    :ensure t
    :bind (("C-x k" . persp-kill-buffer*)
           ("C-x C-b" . persp-ivy-switch-buffer))
    :custom
    (persp-mode-prefix-key (kbd "C-x M-p"))
    :init
    (persp-mode))

#+end_src

** Avy

#+begin_src emacs-lisp 

  (use-package avy
    :config
    (setq avy-all-windows 'all-frames)
    (global-set-key (kbd "C-:") 'avy-goto-char)
    )

#+end_src

** Languages

**** tree-sitter

#+begin_src emacs-lisp 

                                          ; :hook (
                                          ; (org-mode TeX-mode LaTeX-mode typescript-mode
                                          ; maplev-mode c-mode c++-mode python-mode rustic-mode)
                                          ;. tree-sitter-hl-mode))

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
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . typescript)))

  (use-package tree-sitter-langs
    :straight (tree-sitter-langs :type git
                                 :host github
                                 :repo "ubolonton/emacs-tree-sitter"
                                 :files ("langs/*.el" "langs/queries"))
    :after tree-sitter)

#+end_src

*** TypeScript

This is a basic configuration for the TypeScript language so that =.ts= files activate =typescript-mode= when opened.  We're also adding a hook to =typescript-mode-hook= to call =lsp-deferred= so that we activate =lsp-mode= to get LSP features every time we edit TypeScript code.

#+begin_src emacs-lisp

  (use-package typescript-mode
    :mode "\\.ts\\'"
    :hook (typescript-mode . lsp-deferred)
    :config
    (setq typescript-indent-level 2))

#+end_src

*Important note!*  For =lsp-mode= to work with TypeScript (and JavaScript) you will need to install a language server on your machine.  If you have Node.js installed, the easiest way to do that is by running the following command:

#+begin_src shell :tangle no

  npm install -g typescript-language-server typescript

#+end_src

This will install the [[https://github.com/theia-ide/typescript-language-server][typescript-language-server]] and the TypeScript compiler package.

*** C/C++

#+begin_src emacs-lisp

  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)

#+end_src

*** Latex

#+begin_src emacs-lisp 

  (add-hook 'TeX-mode-hook 'lsp)
  (add-hook 'LaTeX-mode-hook 'lsp)

  (add-hook 'TeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  (add-hook 'TeX-mode-hook #'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  (setq-default fill-column 80)

  (add-hook 'TeX-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'LaTeX-mode-hook #'display-fill-column-indicator-mode)

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

#+end_src

*** Python

We use =lsp-mode= and =dap-mode= to provide a more complete development environment for Python in Emacs.  Check out [[https://emacs-lsp.github.io/lsp-mode/page/lsp-pyls/][the =pyls= configuration]] in the =lsp-mode= documentation for more details.

Make sure you have the =pyls= language server installed before trying =lsp-mode=!

#+begin_src sh :tangle no

  pip install --user "python-language-server[all]"

#+end_src

There are a number of other language servers for Python so if you find that =pyls= doesn't work for you, consult the =lsp-mode= [[https://emacs-lsp.github.io/lsp-mode/page/languages/][language configuration documentation]] to try the others!

#+begin_src emacs-lisp

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

#+end_src

You can use the pyvenv package to use =virtualenv= environments in Emacs.  The =pyvenv-activate= command should configure Emacs to cause =lsp-mode= and =dap-mode= to use the virtual environment when they are loaded, just select the path to your virtual environment before loading your project.

#+begin_src emacs-lisp

  (use-package pyvenv
    :after python-mode
    :config
    (pyvenv-mode 1))

#+end_src

*** Maple mode

#+begin_src emacs-lisp

  (defvar maplev-package "/home/jose/maple/toolbox/maplev/maplev-3.0.4.tar")
  (if (file-exists-p maplev-package) (package-install-file maplev-package))

  (add-to-list 'auto-mode-alist '("\\.mpl\\'" . maplev-mode))
  (add-to-list 'auto-mode-alist '("\\.mm\\'" . maplev-mode))

#+end_src

*** Mathematica mode

#+begin_src emacs-lisp 

  (use-package wolfram-mode
    :config
    (setq wolfram-program "/usr/local/bin/MathKernel")
    (setq wolfram-path "~/.Mathematica")
    (add-to-list 'auto-mode-alist '("\\.m\\'" . wolfram-mode))
    (add-to-list 'auto-mode-alist '("\\.wl\\'" . wolfram-mode)))

#+end_src

*** TOML mode
#+begin_src emacs-lisp 

  (use-package toml-mode)

#+end_src

** Projectile

[[https://projectile.mx/][Projectile]] is a project management library for Emacs which makes it a lot easier to navigate around code projects for various languages.  Many packages integrate with Projectile so it's a good idea to have it installed even if you don't use its commands directly.

#+begin_src emacs-lisp

  (use-package projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :custom ((projectile-completion-system 'ivy))
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :init
                                          ; NOTE: Set this to the folder where you keep your Git repos!
    (when (file-directory-p "~/Documents/GithubProjects")
      (setq projectile-project-search-path '("~/Documents/GithubProjects")))
    (setq projectile-switch-project-action #'projectile-dired))

  (use-package counsel-projectile
    :after projectile
    :config (counsel-projectile-mode))

#+end_src

** Git-gutter

#+begin_src emacs-lisp 

                                          ;TODO: https://github.com/emacsorphanage/git-gutter
                                          ;(use-package git-gutter)

#+end_src

** Commenting

Emacs' built in commenting functionality =comment-dwim= (usually bound to =M-;=) doesn't always comment things in the way you might expect so we use [[https://github.com/redguardtoo/evil-nerd-commenter][evil-nerd-commenter]] to provide a more familiar behavior.  I've bound it to =M-/= since other editors sometimes use this binding but you could also replace Emacs' =M-;= binding with this command.

#+begin_src emacs-lisp

  (use-package evil-nerd-commenter
    :bind ("M-/" . evilnc-comment-or-uncomment-lines))

#+end_src

* Terminals

** term-mode

=term-mode= is a built-in terminal emulator in Emacs.  Because it is written in Emacs Lisp, you can start using it immediately with very little configuration.  If you are on Linux or macOS, =term-mode= is a great choice to get started because it supports fairly complex terminal applications (=htop=, =vim=, etc) and works pretty reliably.  However, because it is written in Emacs Lisp, it can be slower than other options like =vterm=.  The speed will only be an issue if you regularly run console apps with a lot of output.

One important thing to understand is =line-mode= versus =char-mode=.  =line-mode= enables you to use normal Emacs keybindings while moving around in the terminal buffer while =char-mode= sends most of your keypresses to the underlying terminal.  While using =term-mode=, you will want to be in =char-mode= for any terminal applications that have their own keybindings.  If you're just in your usual shell, =line-mode= is sufficient and feels more integrated with Emacs.

With =evil-collection= installed, you will automatically switch to =char-mode= when you enter Evil's insert mode (press =i=).  You will automatically be switched back to =line-mode= when you enter Evil's normal mode (press =ESC=).

Run a terminal with =M-x term!=

*Useful key bindings:*

- =C-c C-p= / =C-c C-n= - go back and forward in the buffer's prompts (also =[[= and =]]= with evil-mode)
- =C-c C-k= - Enter char-mode
- =C-c C-j= - Return to line-mode
- If you have =evil-collection= installed, =term-mode= will enter char mode when you use Evil's Insert mode

  #+begin_src emacs-lisp

    (use-package term
      :commands term
      :config
      (setq explicit-shell-file-name "zsh") ; Change this to zsh, etc
                                            ;(setq explicit-zsh-args '())         ; Use 'explicit-<shell>-args for shell-specific args

                                            ; Match the default Bash shell prompt.  Update this if you have a custom prompt
      (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

  #+end_src

*** Better term-mode colors

The =eterm-256color= package enhances the output of =term-mode= to enable handling of a wider range of color codes so that many popular terminal applications look as you would expect them to.  Keep in mind that this package requires =ncurses= to be installed on your machine so that it has access to the =tic= program.  Most Linux distributions come with this program installed already so you may not have to do anything extra to use it.

#+begin_src emacs-lisp

  (use-package eterm-256color
    :hook (term-mode . eterm-256color-mode))

#+end_src

**** shell-mode

[[https://www.gnu.org/software/emacs/manual/html_node/emacs/Interactive-Shell.html#Interactive-Shell][shell-mode]] is a middle ground between =term-mode= and Eshell.  It is *not* a terminal emulator so more complex terminal programs will not run inside of it.  It does have much better integration with Emacs because all command input in this mode is handled by Emacs and then sent to the underlying shell once you press Enter.  This means that you can use =evil-mode='s editing motions on the command line, unlike in the terminal emulator modes above.

*Useful key bindings:*

- =C-c C-p= / =C-c C-n= - go back and forward in the buffer's prompts (also =[[= and =]]= with evil-mode)
- =M-p= / =M-n= - go back and forward in the input history
- =C-c C-u= - delete the current input string backwards up to the cursor
- =counsel-shell-history= - A searchable history of commands typed into the shell

  One advantage of =shell-mode= on Windows is that it's the only way to run =cmd.exe=, PowerShell, Git Bash, etc from within Emacs.  Here's an example of how you would set up =shell-mode= to run PowerShell on Windows:

  #+begin_src emacs-lisp

    (when (eq system-type 'windows-nt)
      (setq explicit-shell-file-name "powershell.exe")
      (setq explicit-powershell.exe-args '()))

  #+end_src

** Eshell

[[https://www.gnu.org/software/emacs/manual/html_mono/eshell.html#Contributors-to-Eshell][Eshell]] is Emacs' own shell implementation written in Emacs Lisp.  It provides you with a cross-platform implementation (even on Windows!) of the common GNU utilities you would find on Linux and macOS (=ls=, =rm=, =mv=, =grep=, etc).  It also allows you to call Emacs Lisp functions directly from the shell and you can even set up aliases (like aliasing =vim= to =find-file=).  Eshell is also an Emacs Lisp REPL which allows you to evaluate full expressions at the shell.

The downsides to Eshell are that it can be harder to configure than other packages due to the particularity of where you need to set some options for them to go into effect, the lack of shell completions (by default) for some useful things like Git commands, and that REPL programs sometimes don't work as well.  However, many of these limitations can be dealt with by good configuration and installing external packages, so don't let that discourage you from trying it!

*Useful key bindings:*

- =C-c C-p= / =C-c C-n= - go back and forward in the buffer's prompts (also =[[= and =]]= with evil-mode)
- =M-p= / =M-n= - go back and forward in the input history
- =C-c C-u= - delete the current input string backwards up to the cursor
- =counsel-esh-history= - A searchable history of commands typed into Eshell

  We will be covering Eshell more in future videos highlighting other things you can do with it.

  For more thoughts on Eshell, check out these articles by Pierre Neidhardt:
  - https://ambrevar.xyz/emacs-eshell/index.html
  - https://ambrevar.xyz/emacs-eshell-versus-shell/index.html

    #+begin_src emacs-lisp

      (defun efs/configure-eshell ()
                                              ; Save command history when commands are entered
        (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

                                              ; Truncate buffer for performance
        (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

                                              ; Bind some useful keys for evil-mode
        (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
        (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
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


    #+end_src

** Vterm

#+begin_src emacs-lisp 

  (use-package vterm
    :commands vterm
    :config
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  
    (setq vterm-shell "zsh")                      
    (setq vterm-max-scrollback 10000))

#+end_src

* File Management

** Dired

Dired is a built-in file manager for Emacs that does some pretty amazing things!  Here are some key bindings you should try out:

*** Key Bindings

**** Navigation

*Emacs* / *Evil*
- =n= / =j= - next line
- =p= / =k= - previous line
- =j= / =J= - jump to file in buffer
- =RET= - select file or directory
- =^= - go to parent directory
- =S-RET= / =g O= - Open file in "other" window
- =M-RET= - Show file in other window without focusing (previewing files)
- =g o= (=dired-view-file=) - Open file but in a "preview" mode, close with =q=
- =g= / =g r= Refresh the buffer with =revert-buffer= after changing configuration (and after filesystem changes!)

**** Marking files

- =m= - Marks a file
- =u= - Unmarks a file
- =U= - Unmarks all files in buffer
- =* t= / =t= - Inverts marked files in buffer
- =% m= - Mark files in buffer using regular expression
- =*= - Lots of other auto-marking functions
- =k= / =K= - "Kill" marked items (refresh buffer with =g= / =g r= to get them back)
- Many operations can be done on a single file if there are no active marks!

**** Copying and Renaming files

- =C= - Copy marked files (or if no files are marked, the current file)
- Copying single and multiple files
- =U= - Unmark all files in buffer
- =R= - Rename marked files, renaming multiple is a move!
- =% R= - Rename based on regular expression: =^test= , =old-\&=

  *Power command*: =C-x C-q= (=dired-toggle-read-only=) - Makes all file names in the buffer editable directly to rename them!  Press =Z Z= to confirm renaming or =Z Q= to abort.

**** Deleting files

- =D= - Delete marked file
- =d= - Mark file for deletion
- =x= - Execute deletion for marks
- =delete-by-moving-to-trash= - Move to trash instead of deleting permanently

**** Creating and extracting archives

- =Z= - Compress or uncompress a file or folder to (=.tar.gz=)
- =c= - Compress selection to a specific file
- =dired-compress-files-alist= - Bind compression commands to file extension

**** Other common operations

- =T= - Touch (change timestamp)
- =M= - Change file mode
- =O= - Change file owner
- =G= - Change file group
- =S= - Create a symbolic link to this file
- =L= - Load an Emacs Lisp file into Emacs

* Applications

** Some App

This is an example of configuring another non-Emacs application using org-mode.  Not only do we write out the configuration at =.config/some-app/config=, we also compute the value that gets stored in this configuration from the Emacs Lisp block above it.

#+NAME: the-value
#+begin_src emacs-lisp :tangle no

  (+ 55 100)

#+end_src

*NOTE*: Set the =:tangle= parameter below to =.config/some-app/config= for this to work!

#+begin_src conf :tangle no :noweb yes

  value=<<the-value()>>

#+end_src

** Runtime Performance

Dial the GC threshold back down so that garbage collection happens more frequently but in less time.

#+begin_src emacs-lisp

                                          ; Make gc pauses faster by decreasing the threshold.
  (setq gc-cons-threshold (* 2 1000 1000))

#+end_src

** Presentation mode with org-tree-slide

#+begin_src emacs-lisp

  (use-package hide-mode-line)

  (defun efs/presentation-setup ()
    (setq text-scale-mode-amount 3)
    (hide-mode-line-mode 1)
    (org-display-inline-images)
    (text-scale-mode 1))

  (defun efs/presentation-end ()
    (hide-mode-line-mode 0)
    (text-scale-mode 0))

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

#+end_src

** System Clipboard

#+begin_src emacs-lisp

  (use-package simpleclip
    :config
    (simpleclip-mode 1))

#+end_src

** Markdown-EWW preview

#+begin_src emacs-lisp 

  (use-package markdown-preview-eww
    :ensure nil
    :straight (
               :host github
               :files ("*.el")
               :repo "niku/markdown-preview-eww"))

#+end_src

** Managing Mail with mu4e

It is necessary to install isync, setup a .mbsyncrc file like the following:

#+begin_src  

IMAPAccount unm
Host outlook.office365.com
User jabelcastellanosjoo@unm.edu
AuthMechs Login
PassCmd +"pass school_email"
Port 993
SSLType IMAPS
CertificateFile /etc/ssl/certs/ca-certificates.crt

IMAPStore unm-remote
Account unm

MaildirStore unm-local
Path ~/Mail/unm/
Inbox ~/Mail/unm/Inbox
SubFolders Verbatim

Channel unm
Far :unm-remote:
Near :unm-local:
Patterns *
Create Both
Expunge Both
SyncState *

IMAPAccount cs-unm
Host snape.cs.unm.edu
User jose.castellanosjoo@cs.unm.edu
AuthMechs Login
PassCmd +"pass school_work_email"
Port 993
SSLType IMAPS
CertificateFile /etc/ssl/certs/ca-certificates.crt

IMAPStore cs-unm-remote
Account cs-unm

MaildirStore cs-unm-local
Path ~/Mail/cs-unm/
Inbox ~/Mail/cs-unm/Inbox
SubFolders Verbatim

Channel cs-unm
Far :cs-unm-remote:
Near :cs-unm-local:
Patterns *
Create Both
Expunge Both
SyncState *

#+end_src

Additionally, install mu and set it up with the following:

#+begin_src

mu init --maildir=~/Mail --my-address=ADDRESS1 --my-address=ADDRESS2
mu index

#+end_src
