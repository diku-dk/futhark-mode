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
Alternatively, add the following lines to your Emacs init file:

    (add-to-list 'load-path "path/to/futhark-mode")
    (require 'futhark-mode)

## Usage

This mode is pretty straightforward and does not provide a lot of
bells and whistles.  But it does have some commands:

  * **C-c C-l** (`futhark-load-file`) loads the current file into
    `futharki`, creating a new instance if a current one does not
    exist.

Also consider `M-x flycheck-mode` to get immediate information about
type- and syntax errors.

## Testing

Run `futhark-mode.el-corpus-test.sh` to test if the auto-indentation
works.  If you find a piece of code that futhark-mode cannot indent
properly, please fix it, and put a small example in the
`futhark-mode.el-corpus` directory to ensure that it doesn't break in
the future.

## Authors

Initiated by Troels Henriksen in 2013.  Subsequently improved by:

  + Niels G. W. Serup
  + Troels Henriksen
  + Rasmus Wriedt Larsen
