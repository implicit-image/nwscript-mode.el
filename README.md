# nwscript-mode.el
Emacs major mode for editing Bioware's [NWScript](https://nwnlexicon.com/index.php/Main_Page "Neverwinter Script").

# features

This major mode implements following features:
- syntax highlighting
- `imenu` support for finding constants, structs, function declarations and definitions.
- completion support for include files, see documentation of `nwscript-include-root` and `nwscript-include-dirs`.
- code completion support using `dabbrev-capf`, optionally uses `cape-dabbrev` or `company-dabbrev-code` if available.

# TODO:
- [ ] fully correct indentation
- [ ] lsp-mode support
