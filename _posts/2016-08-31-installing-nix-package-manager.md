---
layout: post
title: "Installing the Nix package manager"
date_granularity: day
tags: nixos
---

A minimal guide to getting started with
[the Nix package manager](https://nixos.org/nix/)
on any Linux or OSX machine.

There is a lot to explain about Nix that I am intentionally omitting here.
This is only what you need to know to get a working installation.

## 1. Install Nix

This first step requires you to be a sudoer (it will prompt you for your
password), but *do not* type "sudo" yourself.
It's important for all of these steps to be performed as your own user,
not as root.

Run this command:

{% highlight bash %}
curl https://nixos.org/nix/install | sh
{% endhighlight %}

This creates the `/nix` directory, which is owned by root.
Nix manages absolutely everything in this directory; you should never touch it.

## 2. Choose a channel

Channels are stable sets of packages.
Go to <a href="https://nixos.org/channels/">nixos.org/channels</a> and find
the highest-numbered channel.

Channels are released every six months. At the time of writing, "nixos-16.03"
(from March 2016) is the latest channel, so that's what we'll use in the
following steps; but you should replace `16.03` with the version you're using.

## 3. Set up environment variables

Add the following lines to the end of `~/.bashrc` (or `~/.zshrc`, or whatever
the config file for the shell you use):

{% highlight bash %}
source $HOME/.nix-profile/etc/profile.d/nix.sh
export NIX_PATH="nixpkgs=$HOME/.nix-defexpr/channels/nixos-16.03"
{% endhighlight %}

Remember to replace `16.03` with the actual channel you're using.

## 4. Add the channel

Run the following commands in a new shell, because you'll need the `.bashrc`
changes made by step 3:

{% highlight bash %}
nix-channel --add http://nixos.org/channels/nixos-16.03 nixos-16.03
nix-channel --update
{% endhighlight %}

Again, replace `16.03` with the channel you're using.

## 5. Play

Run this to show that it's working!

{% highlight bash %}
nix-shell -p rogue --command rogue
{% endhighlight %}

## Uninstall

If you want to remove everything related to Nix:

{% highlight bash %}
sudo rm -rf /nix
rm -rf ~/.nix-*
{% endhighlight %}
