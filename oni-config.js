const activate = (oni) => {
  oni.input.bind("<c-q>", () => oni.configuration.setValues({
      "editor.quickInfo.enabled": !oni.configuration.getValue("editor.quickInfo.enabled")
  }));

  oni.input.unbind("<s-c-b>");
  // Re-binding the sidebar.toggle is done in oni-config.vim

  oni.input.unbind("<c-g>") // make C-g work as expected in vim
  oni.input.bind("<s-c-g>", () => oni.commands.executeCommand("sneak.show")) // Rebind Oni's behavior
};

module.exports = {
  activate,
  "colorscheme": "alduin",
  "oni.hideMenu": true,
  "oni.useDefaultConfig": false,
  "oni.loadInitVim": "~/.vimrc",
  "editor.fontSize": "11px",
  "editor.fontFamily": "Mono",
  "editor.completions.enabled": true,
  // We can enable the quickInfo on demand with the keybinding in activate above
  "editor.quickInfo.enabled": false,
  "autoClosingPairs.enabled": true
}
