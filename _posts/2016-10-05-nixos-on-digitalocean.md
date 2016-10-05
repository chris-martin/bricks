---
layout: post
title: "NixOS on DigitalOcean"
date_granularity: day
tags: nixos
---

How to start a new NixOS droplet on Digital Ocean using
[nixos-infect](https://github.com/elitak/nixos-infect):

* Start an Ubuntu 16.04 droplet
* Enable ipv6
* Add your key to `~/.ssh/authorized_keys` and SSH in as root
* `wget https://raw.githubusercontent.com/elitak/nixos-infect/master/nixos-infect`
* `bash nixos-infect`
* NixOS will install (this takes a little while) and the machine will reboot.
* SSH in as root again
* Update `/etc/nixos/configuration.nix` to set up things like SSH and users
* `nixos-rebuild switch`
* Use `passwd` to set user passwords (remember to do this for at least one
  sudoer so you don't lock yourself out of the machine!)
