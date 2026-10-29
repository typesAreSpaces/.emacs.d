;;; early-init.el --- -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Skip garbage collection while loading; init.el restores a moderate
;; threshold in `emacs-startup-hook'.
(setq gc-cons-threshold most-positive-fixnum)

(setq native-comp-deferred-compilation nil)
(setq native-comp-jit-compilation nil)
(setq comp-enable-subr-trampolines nil)
