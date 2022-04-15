# futhark-mode for Emacs

[![MELPA](https://melpa.org/packages/futhark-mode-badge.svg)](https://melpa.org/#/futhark-mode)[![CI](https://github.com/diku-dk/futhark-mode/workflows/CI/badge.svg)](https://github.com/diku-dk/futhark-mode/actions)

See the file `futhark-mode.el`.  This Emacs mode provides:

  * syntax highlighting
  * automatic indentation
  * interaction with an inferior `futhark repl` process

For more features, try [Eglot](https://github.com/joaotavora/eglot),
which works well with Futhark's built-in LSP server `futhark lsp`.

## Installation

You can install this mode with Emacs' package manager.  Enable the
https://melpa.org/ archive, and install the `futhark-mode` package.
*Alternatively*, add the following lines to your Emacs init file:

    (add-to-list 'load-path "path/to/futhark-mode")
    (require 'futhark-mode)

## Usage

This mode is pretty straightforward and does not provide a lot of
bells and whistles.  But it does have some commands:

  * **C-c C-l** (`futhark-load-file`) loads the current file into
    `futhark repl`, creating a new instance if a current one does not
    exist.

## Testing

Run `tools/test-indentation` to test if the auto-indentation works.  If
you find a piece of code that futhark-mode cannot indent properly,
please fix it, and put a small example in the `tools/test-corpus`
directory to ensure that it doesn't break in the future.

Note that not all of the indentation examples in `tools/test-corpus` are
necessarily *ideal*.  Some of them exemplify behaviour that we would
like to fix (but exist as a regression test to avoid even worse
outcomes).

Additionally, you can symlink the `tools/git-hooks/pre-commit` hook into
your local `.git/hooks` directory to automatically check your changes
before each commit.

## Authors

  + Niels G. W. Serup
  + Rasmus Wriedt Larsen
  + Troels Henriksen

## Reloading when hacking on the code

Add the directory containing the `.el` files to your load path with

    (add-to-list 'load-path "/.../futhark-mode")
    (require 'futhark-dev)

and then enter `M-x RET futhark-dev-reload RET` whenever needed.
