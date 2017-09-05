{ html, scss, code }:

let
  p  = x: html.p {} (markdown x);
  li = x: html.li {} (markdown x);

  bash = code { language = "bash"; };

in {
  title = "NixOS on DigitalOcean";
  date  = "2016 Oct 5";
  slug  = "nixos-on-digitalocean";

  css = scss ./nixos-digitalocean.scss;

  abstract = ''
    How to start a new NixOS droplet on Digital Ocean using nixos-infect.
  '';

  body = [
    (p ''
      How to start a new NixOS droplet on Digital Ocean using
      [nixos-infect](https://github.com/elitak/nixos-infect):
    '')

    (html.ol {} [

      (li "Start an Ubuntu 16.04 droplet.")

      (li "Enable ipv6.")

      (li "Add your key to `~/.ssh/authorized_keys` and SSH in as root.")

      (li [
        "Download nixos-infect:"
        (bash ''
          wget https://raw.githubusercontent.com/elitak/nixos-infect/master/nixos-infect
        '')
      ])

      (li [
        "Run nixos-infect:"
        (bash "bash nixos-infect")
      ])

      (li ''
        NixOS will install (this takes a little while) and the machine will
        reboot.
      '')

      (li "SSH in as root again.")

      (li ''
        Update `/etc/nixos/configuration.nix` to set up things like
        SSH and users.
      '')

      (li [
        "Rebuild NixOS:"
        (bash "nixos-rebuild switch")
      ])

      (li ''
        Use `passwd` to set user passwords. Remember to do this for at least one
        sudoer so you don’t lock yourself out of the machine!
      '')

    ])

  ];
}
