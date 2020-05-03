with (import <nixpkgs> {});

mkShell {
  buildInputs = [
    elixir
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-language-server
    expect
    nodejs
  ];
}
