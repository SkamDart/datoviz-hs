{
  description = "Almost raw Haskell bindings to datoviz.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "datoviz-hs";

        datoviz = pkgs.callPackage datoviz-derivation {};

        datoviz-derivation =
          { stdenv, cglm, cmake, fetchFromGitHub, glfw, shaderc, vulkan-headers, vulkan-loader, zlib }:
          stdenv.mkDerivation
          {
             name = "datoviz";
             version = "0.0.1";
             description =
               "High-performance GPU interactive scientific data visualization with Vulkan.";
             src = fetchFromGitHub ({
                 owner = "datoviz";
                 repo = "datoviz";
                 rev = "694b14c92145abbb1c834ba4504487b4f916d14d";
                 sha256 = "sha256-RRE5ilGDtZUAmdAGjaxIybZsCX7BZfovRWu+6PEhVsE=";
                 fetchSubmodules = true;
             });
             patches = [ ./cmake-fix.patch ];
             buildDepends = [ stdenv ];
             buildInputs = [ cglm glfw shaderc vulkan-headers vulkan-loader zlib ];
             nativeBuildInputs = [ cmake ];
          };

        derivation =
          { mkDerivation
          , base
          , cglm
          , glfw
          , lib
          , linear
          , random
          , shaderc
          , vector
          , vulkan-headers
          , vulkan-loader
          , xorg
          }:
          mkDerivation {
            pname = packageName;
            version = "0.1.0.0";
            src = ./.;
            libraryHaskellDepends =
              [
                base
                linear
                random
                vector
              ];
            librarySystemDepends =
            [
              cglm datoviz glfw shaderc vulkan-headers vulkan-loader
              # xorg.libX11 xorg.libXrandr xorg.libXinerama xorg.libXcursor
              # xorg.libXi xorg.libXau xorg.libXdmcp xorg.libXrender xorg.libXfixes
            ];
            description = "Almost raw Haskell bindings to datoviz.";
            license = "unknown";
            hydraPlatforms = lib.platforms.none;
          };

        pkg = haskellPackages.callPackage derivation {};

      in {
        packages.${packageName} = pkg;

        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          packages = p: [ pkg ];
          buildInputs = with haskellPackages; pkg.env.buildInputs ++ [
            cabal-install
            haskell-language-server
            hoogle
          ];
          # https://discourse.nixos.org/t/shared-libraries-error-with-cabal-repl-in-nix-shell/8921/9
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath pkg.buildInputs;
        };

      });
}

