# nwscript-mode.el
Emacs major mode for editing Bioware's [NWScript](https://nwnlexicon.com/index.php/Main_Page "Neverwinter Script").

# features

This major mode implements following features:
- syntax highlighting
- single and multi line comments
- `imenu` support for finding constants, structs, function declarations and definitions.
- completion support for include files, see documentation of `nwscript-include-root` and `nwscript-include-dirs`.
- code completion support using `dabbrev-capf`, optionally uses `cape-dabbrev` or `company-dabbrev-code` if available.
- indenting supports:
  + multi-line function parameter lists
  + multi-line function calls
  + multi-line expression in if statement tests
# TODO:
- [-] fully correct indentation (atm indentation works, but is still buggy and the code is messy)
- [ ] lsp-mode support
