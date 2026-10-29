;;; early-init.el --- -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Apply the configured frame chrome before the first frame is created.
;; init.el still disables the corresponding global modes for later frames.
(dolist (parameter '((menu-bar-lines . 0)
                     (tool-bar-lines . 0)
                     (vertical-scroll-bars . nil)
                     (left-fringe . 10)
                     (right-fringe . 10)))
  (add-to-list 'default-frame-alist parameter))

;; Skip garbage collection while loading; init.el restores a moderate
;; threshold in `emacs-startup-hook'.
(setq gc-cons-threshold most-positive-fixnum)

(setq native-comp-deferred-compilation nil)
(setq native-comp-jit-compilation nil)
(setq comp-enable-subr-trampolines nil)
