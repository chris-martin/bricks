{ html-tags, scss, code }:

let
  p  = x: html-tags.p  (markdown x);
  li = x: html-tags.li (markdown x);

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
    (html "<ol>")
    (li "Start an Ubuntu 16.04 droplet.")
    (li "Enable ipv6.")
    (li "Add your key to `~/.ssh/authorized_keys` and SSH in as root.")
    (li "Download nixos-infect:")
    (html "</ol>")
    (bash ''
      wget https://raw.githubusercontent.com/elitak/nixos-infect/master/nixos-infect
    '')
    (html ''<ol start="5">'')
    (li "Run nixos-infect:")
    (html "</ol>")
    (bash "bash nixos-infect")
    (html ''<ol start="6">'')
    (li ''
      NixOS will install (this takes a little while) and the machine will
      reboot.
    '')
    (li "SSH in as root again.")
    (li ''
      Update `/etc/nixos/configuration.nix` to set up things like
      SSH and users.
    '')
    (li "Rebuild NixOS:")
    (html "</ol>")
    (bash "nixos-rebuild switch")
    (html ''<ol start="10">'')
    (li ''
      Use `passwd` to set user passwords. Remember to do this for at least one
      sudoer so you don’t lock yourself out of the machine!
    '')
    (html "</ol>")
  ];
}
