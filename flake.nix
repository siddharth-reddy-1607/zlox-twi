{
  description = "zlox Tree Walk Interpreter";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
    in {
       devShells.${system}.default = pkgs.mkShell{
           name = "Zig Devshell";
           packages = with pkgs; [
               zig
               zsh
           ]; 
           shellHook = ''
            echo "Zig Devshell using version: $(zig version)"
            exec zsh
           '';
       }; 
    };
}
