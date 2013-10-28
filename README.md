## Emacs Projector

`projector.el` is a lightweight Emacs library for managing project/repository-aware shell and shell command buffers. It has no external dependencies but uses `ido-mode` for project, buffer, and shell command completion suggestions.

It can spawn both synchronous and asynchronous buffers for shell-commands as well as dedicated `shell-mode` buffers for repositories. Shell command completion candidate suggestions are served from `shell-command-history` in the minibuffer.

For async processes, the exit messaging will be sent to [`terminal-notifier`](https://github.com/alloy/terminal-notifier) or [`growlnotify`](http://growl.info/downloads) if either program is available in your `exec-path`, with fallback to a normal Emacs `(message)` notification.

## Installation

To install, drop `projector.el` in your load path and add `(require 'projector)` to your initialization and set the variable `projector-projects-root` to the root folder for your local projects/repositories:

```
(require 'projector)  
(setq projector-projects-root "~/code/")
```

## Usage

Then the following functions are available:

* `(projector-run-shell-command-project-root)` - Run the named shell command from the current repository root in a dedicated buffer. With the `C-u` prefix, run the process in the background and output on exit to `terminal-notifier`, `growlnotify`, or Emacs.

* `(projector-run-shell-command-project-root-background)` - Same as running `(projector-run-shell-command-project-root)` with the `C-u` prefix.


* `(projector-run-shell-command-current-directory)` - Run the named shell command from the current directory in a dedicated buffer. With the `C-u` prefix, run the process in the background and output on exit to `terminal-notifier`, `growlnotify`, or Emacs.

* `(projector-run-shell-command-current-directory-background)` - Same as running `(projector-run-shell-command-current-directory)` with the `C-u` prefix.

* `(projector-switch-to-or-create-project-shell)` - Find or create a dedicated shell for the current repository.

* `(projector-open-project-shell)` - Use `ido` to find or create a dedicated shell for a project/repository in your `projector-projects-root`.

* `(projector-switch-to-shell-buffer)` - Use `ido` to switch to any shell buffer opened by `projector`.

* `(projector-switch-to-shell-buffer-in-project)` - Use `ido` to switch to any shell buffer opened by `projector` in the current project/repository.

I will leave the key-binding of these up to you, or you can just call them with `M-x` if you'd prefer.







