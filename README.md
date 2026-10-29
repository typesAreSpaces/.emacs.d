# .emacs.d
My emacs config files

Edit `config.org`, then tangle it to regenerate `init.el`. `early-init.el`
is maintained separately.

Startup activates installed package autoloads without reading archive indexes.
Package installation and explicit update commands still fetch metadata when
needed. Org, AUCTeX, LSP, and optional tools load on their existing mode or
command triggers. Global editing modes, the dashboard, clipboard integration,
browser editing server, and workflow restoration remain configured at startup.
Org protocol handling is installed before the first client file request.

To profile init evaluation using installed packages, without starting servers,
accessing the network, or changing saved workflows:

```sh
emacs -Q --batch -l scripts/benchmark-startup.el
```

To also check global editing modes and deferred configuration on first use:

```sh
EMACS_BENCH_CHECK="$PWD/scripts/check-startup.el" \
  emacs -Q --batch -l scripts/benchmark-startup.el
```

The benchmark prints JSON with timings, errors, loaded features, and the slowest
forms. Set `EMACS_BENCH_REPORT` to write that report to a file. Use
`EMACS_BENCH_INIT` and `EMACS_BENCH_EARLY` to measure saved copies of an older
configuration against the same installed packages.

On Emacs 31.1, three sequential warm runs measured median init evaluation of
2.747 seconds before optimization and 0.757 seconds afterward (72% reduction;
667 versus 438 loaded features). Both first-use checks passed. These are batch
source-loading measurements with Straight in safe mode; they exclude GUI
creation/redisplay, package builds, startup hooks/workflow restoration, idle
timers, and first-use latency. Deferred tools pay their loading cost on use.

Workflow commands live under `SPC j`: save (`s`), load (`l`), change (`c`),
new (`n`), delete (`d`), and rename (`r`). Change saves the current workflow
before loading the next one; load replaces it without saving its layout.
Automatic saving remains opt-in through `my/session-autosave`.

`rej` starts an unnamed blank workspace without creating a `session-N` folder.
Use save (`SPC j s`) to give it a name, or load an existing workflow. Explicitly
creating a new session (`SPC j n`) still creates a saved workflow.

Workflow loading restores all saved buffers before applying tabs and window
layouts. Large workflows may take longer to open, since file modes initialize
before loading finishes. Active workflows keep their desktop lock; abandoned
locks from exited Emacs processes are ignored. A desktop modified by another
process is checked before overwriting it.

Run the isolated workflow checks with installed packages (temporary files only):

```sh
emacs -Q --batch -l scripts/check-workflows.el
```
