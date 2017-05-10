--------------------------------------------------------------------------------
title:    NixOS on DigitalOcean
date:     2016 Oct 5
css:      nixos-digitalocean.scss
slug:     nixos-on-digitalocean
abstract: How to start a new NixOS droplet on Digital Ocean using nixos-infect.
--------------------------------------------------------------------------------

How to start a new NixOS droplet on Digital Ocean using
[nixos-infect](https://github.com/elitak/nixos-infect):

<ol>

<li>Start an Ubuntu 16.04 droplet.</li>

<li>Enable ipv6.</li>

<li>
    Add your key to <code>~/.ssh/authorized_keys</code> and SSH in as root.
</li>

<li>Download nixos-infect:</li>

</ol>

```bash
wget https://raw.githubusercontent.com/elitak/nixos-infect/master/nixos-infect
```

<ol start="5">

<li>Run nixos-infect:</li>

</ol>

```bash
bash nixos-infect
```

<ol start="6">

<li>
    NixOS will install (this takes a little while) and the machine will reboot.
</li>

<li>SSH in as root again.</li>

<li>
    Update <code>/etc/nixos/configuration.nix</code> to set up things like
    SSH and users.
</li>

<li>Rebuild NixOS:</li>

</ol>

```bash
nixos-rebuild switch
```

<ol start="10">

<li>
    Use `passwd` to set user passwords. Remember to do this for at least one
    sudoer so you don’t lock yourself out of the machine!
</li>

</ol>
