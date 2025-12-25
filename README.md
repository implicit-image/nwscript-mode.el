# nwscript-mode.el
Emacs major mode for editing Bioware's [NWScript](https://nwnlexicon.com/index.php/Main_Page "Neverwinter Script").

# installation

### package.el
From emacs 30.2, you can do:
```elisp
(use-package nwscript-mode
  :vc ( :url "https://github.com/implicit-image/nwscript-mode.el"
        :branch "main"))
```


### straight.el
```elisp
(use-package nwscript-mode
  :straight (nwscript-mode :type git
                           :host github
                           :repo "implicit-image/nwscript-mode.el"))
```

# features

This major mode implements following features:
- syntax highlighting
- single and multi line comments
- `imenu` support for finding constants, structs, function declarations and definitions.
- completion support for include files, see documentation of `nwscript-include-root` and `nwscripti-include-dirs`.
- flymake backend in [nwscript-flymake.el](./nwscript-flymake.el), see `nwscript-enable-flymake` (quickly compiles current file with [nwnsc](https://github.com/nwneetools/nwnsc))
- compilation mode support in [nwscript-compile.el](./nwscript-compile.el), see `nwscript-enable-compile`
- code completion support using `dabbrev-capf`, optionally uses `cape-dabbrev` or `company-dabbrev-code` if available.
- indenting supports:
  + multi-line function parameter lists
  + multi-line function calls
  + multi-line expression in if statement tests


## include files
If you have the game scripts unpacked you can add the path to `nwscript-include-dirs`. See the variables documentation for more details.
```elisp
(add-to-list 'nwscript-include-dirs "/home/user/misc/game-includes")
```


# TODO:
- [-] fully correct indentation (atm indentation works, but is still buggy and the code is messy)
- [ ] lsp-mode support
