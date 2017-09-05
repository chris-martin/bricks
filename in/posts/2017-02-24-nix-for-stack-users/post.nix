{ code, markdown, html-tags, file-path }:

let
  p  = x: html-tags.p  (markdown x);
  li = x: html-tags.li (markdown x);
  h2 = x: html-tags.h2 (markdown x);

  inherit (html-tags) ul;

  bash = code { language = "bash"; };
  nix  = code { language = "nix";  };
  yaml = code { language = "yaml"; };

  url = {
    nix         = "https://nixos.org/nix/";
    stack       = "https://docs.haskellstack.org/en/stable/README/";
    integration = "https://github.com/commercialhaskell/stack/blob/master/doc/nix_integration.md";
    sass        = "http://sass-lang.com/";
    hakyll      = "https://jaspervdj.be/hakyll/";
    nixos       = "https://nixos.org/";
  };

in {
  title = "Introduction to Nix (for users of Stack)";
  date  = "2017 Feb 24";
  slug  = "nix-for-stack-users";

  thumbnail = file-path ./thumb.png;

  abstract = ''
    What Nix is for, and how it can help you as a Haskell developer.
  '';

  body = [

    (h2 "Install Nix")

    (bash ''
      curl https://nixos.org/nix/install | sh
      source ~/.nix-profile/etc/profile.d/nix.sh
    '')

    (p "It can live happily in any Linux system.")

    ###################################################################

    (h2 "Nix is like Stack")

    (p ''
      Think of [Nix](${url.nix}) like [Stack](${url.stack}), but with a
      much broader scope.
    '')

    (p ''
      Stack is there to manage your project’s dependencies, and help you
      run their programs and compile stuff with them, but only if the
      dependencies are Haskell packages.
    '')

    (p "Nix dependencies can be… kinda any software at all.")

    (p ''
      Nix and Stack work together nicely. When you enable [Nix integration
      for Stack](${url.integration}), you can add any software as a
      dependency of your Stack project.
    '')

    (p ''
      So if you want to, for example, add a [SASS](${url.sass}) compilation
      step to your [Hakyll](${hakyll}) project, your coworkers don’t have
      to do any work to get the SASS compiler. Stack will just install it
      for them.
    '')

    (p ''
      Like Stack keeps a cache of all the stuff it has installed in `~/.stack`
      in your home directory, Nix keeps all of its stuff in `/nix/store`.
      (It’s not in your home directory, but at the root of the filesystem —
      for *technical reasons* — which is why you are prompted to sudo when
      you install it.)
    '')

    ###################################################################

    (h2 "`nix-shell` is like `stack exec`")

    (p ''
      Another comparison between Stack and Nix: You know how you can use Stack
      (outside the context of any project) to run executables from Haskell
      packages?
    '')

    (code {} ''
      > stack exec --package pandoc -- pandoc --version
      pandoc 1.19.2.1
    '')

    (p "`nix-shell` is like `stack exec`.")

    (p ''
      So take for example… Let’s assume you don’t have NPM installed. And
      let’s say you don’t really want to install it permanently or whatever
      because you’re probably not going to need it again, but you just want
      to run it for some one-off command.
    '')

    (code {} ''
      > nix-shell -p nodePackages.npm --run 'npm --version'
      4.1.1
    '')

    (p ''
      `-p npm` is like `--package pandoc`; it tells Nix what packages you
      want available in the sandbox you’re going to run a command in. And
      then `npm --version` is the command you want to run. So that’ll
      download NPM and run `npm --version`, without actually “installing”
      NPM — without modifying your system visibly at all.
    '')

    ###################################################################

    (h2 "`shell.nix` is like `stack.yaml`")

    (p ''
      So then, also like Stack, you can use it in a sort of standalone manner
      like that, or you can have a config file for a project. Here’s one I’m
      using right now:
    '')

    (nix ''
      { pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

      pkgs.haskell.lib.buildStackProject {
        name = "joyofhaskell.com";
        inherit ghc;
        buildInputs = with pkgs; [ sassc zlib ];
        LANG = "en_US.UTF-8";
        TMPDIR = "/tmp";
      }
    '')

    (p ''
      This file is called `shell.nix` and it sits in the root of a Hakyll
      project repository alongside `stack.yaml`.
    '')

    (p "The most important line here is the one with our dependencies:")

    (nix "buildInputs = with pkgs; [ sassc zlib ];")

    (p "And then at the end of our `stack.yaml` is")

    (yaml ''
      nix:
        enable: true
        pure: true
        shell-file: shell.nix
    '')

    (p ''
      which tells Stack to do everything that it does in the context of that
      Nix configuration, which it does by using `nix-shell` under the hood.
    '')

    ###################################################################

    (h2 "`nix-env` is like `apt-get`")

    (p ''
      You can also use Nix as an alternative to Ubuntu’s package manager
      (`apt-get`) for a lot of things if you so choose. (I’m assuming you’re
      using Ubuntu.)
    '')

    (p "For this we use another command, `nix-env`.")

    (p "The basic usage `of nix-env` is:")

    (ul [
      (li "`nix-env -i npm` — to install (e.g. NPM)")
      (li "`nix-env -e npm` — to uninstall")
      (li "`nix-env -q` — to list which package you have installed")
    ])

    (p ''
      You have no pressing need to use `nix-env` instead of your existing
      package manager at the moment, but it will be important if you ever
      use [NixOS](${url.nixos}).
    '')

    (p ''
      An interesting difference between `nix-env` and `apt-get` is that it
      only affects your user, which is why you don’t have to sudo to run it.
    '')
    
  ];
}
