`projector` is a lightweight Emacs library for managing project-aware shell and shell command buffers. It leverages the great [`projectile`](https://github.com/bbatsov/projectile) project interaction library. A quick overview of features:

* It can spawn both synchronous and asynchronous buffers for shell-commands as well as dedicated `shell-mode` buffers for repositories.

* It offers project, buffer, and shell command completion suggestions. Shell command completion candidate suggestions are served from `shell-command-history` in the minibuffer.

* For async processes, it uses [`alert`](https://github.com/jwiegley/alert) for handling the exit message. This makes it easy to hook into notification programs like [`terminal-notifier`](https://github.com/alloy/terminal-notifier) or [`growlnotify`](http://growl.info/downloads).

## Installation

Install it from [MELPA](http://melpa.milkbox.net) or just drop `projector.el` in your load path and add `(require 'projector)` to your initialization.

Optionally, set `alert-default-style` to [one of these](https://github.com/jwiegley/alert/blob/master/alert.el#L123-L128).

Example setup:

``` elisp
(require 'projector)  
(setq alert-default-style 'notifier)
(setq projector-always-background-regex '("^mysql.server\\.*" "^powder\\.*"))
(setq projector-command-modes-alist '(("^heroku run\\.*" . inf-ruby-mode)))
```

## Available Options

###### `projector-always-background-regex`

You can set this to a list of regex patterns for commands that should
always run in the background and will `alert` on completion.

###### `projector-command-modes-alist`

An alist of command patterns to run in specific modes. The alist
should follow the format of `(COMMAND-REGEX . MODE)`.

###### `projector-default-command`

The default command to run with
`projector-run-default-shell-command`. This is usually most helpful to
set on a directoy local level via
[`.dir-locals.el`](http://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Local-Variables.html):

``` elisp
((nil . ((projector-default-command . "foreman start"))))
```

## Available Commands

###### `(projector-run-shell-command-project-root)`

Run the named shell command from the current repository root in a
dedicated buffer. With the `C-u` prefix, run the process in the
background and output on exit to `alert`.

###### `(projector-run-shell-command-project-root-background)`

Same as running `(projector-run-shell-command-project-root)` with the `C-u` prefix.

###### `(projector-run-shell-command-current-directory)`

Run the named shell command from the current directory in a dedicated
buffer. With the `C-u` prefix, run the process in the background and
output on exit to `alert`.

###### `(projector-run-shell-command-current-directory-background)`

Same as running `(projector-run-shell-command-current-directory)` with the `C-u` prefix.

###### `(projector-run-default-shell-command)`

Execute `projector-default-command` at the project root in a dedicated
buffer. With the `C-u` prefix, run the process in the background and
output on exit to `alert`.

###### `(projector-switch-to-or-create-project-shell)`

Find or create a dedicated `shell-mode` buffer for the current repository.

###### `(projector-open-project-shell)`

Find or create a dedicated `shell-mode` buffer for a project/repository in your `projector-projects-root`.

###### `(projector-switch-to-shell-buffer)`

Switch to any shell buffer created by `projector`.

###### `(projector-switch-to-shell-buffer-in-project)`

Switch to any shell buffer created by `projector` in the current
project/repository.

I will leave the key-binding of these up to you, or you can just call
them with `M-x` if you'd prefer.
