{ html, markdown, code }:

let
  p = x: html.p {} (markdown x);
  h2 = html.h2 {};
  bash = code { language = "bash"; };

in {
  title = "Installing the Nix package manager";
  date  = "2016 Aug 31";
  slug  = "installing-nix-package-manager";

  abstract = ''
    A minimal guide to getting started with the Nix package manager
    on any Linux or OSX machine.
  '';

  body = [

    (p ''
      A minimal guide to getting started with
      [the Nix package manager](https://nixos.org/nix/)
      on any Linux or OSX machine.
    '')
    (p ''
      There is a lot to explain about Nix that I am intentionally omitting here.
      This is only what you need to know to get a working installation.
    '')

    #####################################################################

    (h2 "1. Install Nix")

    (p ''
      This first step requires you to be a sudoer (it will prompt you for your
      password), but *do not* type "sudo" yourself.
      It’s important for all of these steps to be performed as your own user,
      not as root.
    '')

    (p "Run this command:")

    (bash "curl https://nixos.org/nix/install | sh")

    (p ''
      This creates the `/nix` directory, which is owned by root.
      Nix manages absolutely everything in this directory; you should never
      touch it.
    '')

    ####################################################################

    (h2 "2. Choose a channel")

    (p ''
      Channels are stable sets of packages.
      Go to [nixos.org/channels](https://nixos.org/channels/)
      and find the highest-numbered channel.
    '')

    (p ''
      Channels are released every six months. At the time of writing,
      "nixos-16.03" (from March 2016) is the latest channel, so that’s what
      we’ll use in the following steps; but you should replace `16.03` with
      the version you’re using.
    '')

    ####################################################################

    (h2 "3. Set up environment variables")

    (p ''
      Add the following lines to the end of `~/.bashrc` (or `~/.zshrc`, or
      whatever the config file for the shell you use):
    '')

    (bash ''
      source $HOME/.nix-profile/etc/profile.d/nix.sh
      export NIX_PATH="nixpkgs=$HOME/.nix-defexpr/channels/nixos-16.03"
    '')

    (p "Remember to replace `16.03` with the actual channel you’re using.")

    ####################################################################

    (h2 "4. Add the channel:")

    (p ''
      Run the following commands in a new shell, because you’ll need the
      `.bashrc` changes made by step 3:
    '')

    (bash ''
      nix-channel --add http://nixos.org/channels/nixos-16.03 nixos-16.03
      nix-channel --update
    '')

    (p "Again, replace `16.03` with the channel you’re using.")

    ####################################################################

    (h2 "5. Play")

    (p "Run this to show that it’s working!")

    (bash "nix-shell -p rogue --command rogue")

    (h2 "Uninstall")

    (p "If you want to remove everything related to Nix:")

    (bash ''
      sudo rm -rf /nix
      rm -rf ~/.nix-*
    '')

  ];
}
