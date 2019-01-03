# futhark-mode for Emacs

[![MELPA](https://melpa.org/packages/futhark-mode-badge.svg)](https://melpa.org/#/futhark-mode)[![Build Status](https://travis-ci.org/diku-dk/futhark-mode.svg?branch=master)](https://travis-ci.org/diku-dk/futhark-mode)

See the file `futhark-mode.el`.  This Emacs mode provides:

  * syntax highlighting
  * automatic indentation
  * interaction with an inferior `futharki` process
  * flycheck definition

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
    `futharki`, creating a new instance if a current one does not
    exist.

Also consider `M-x flycheck-mode` to get immediate information about
type- and syntax errors (requires installation of the `flycheck` Emacs
package).

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

and then evaluate

    (progn
      (ignore-errors (unload-feature 'futhark-highlight t))
      (ignore-errors (unload-feature 'futhark-comint t))
      (ignore-errors (unload-feature 'futhark-flycheck t))
      (ignore-errors (unload-feature 'futhark-const t))
      (ignore-errors (unload-feature 'futhark-smie t))
      (ignore-errors (unload-feature 'futhark-mode t))
      (require 'futhark-mode))
