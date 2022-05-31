#!/bin/sh
exec cabal v2-repl --ghc-option='-fobject-code' emacs-hacker
