--------------------------------------------------------------------------------
title:    Introduction to Nix (for users of Stack)
date:     2017 Feb 24
slug:     nix-for-stack-users
abstract: What Nix is for, and how it can help you as a Haskell developer.
--------------------------------------------------------------------------------

## Install Nix

```bash
curl https://nixos.org/nix/install | sh
source ~/.nix-profile/etc/profile.d/nix.sh
```

It can live happily in any Linux system.

## Nix is like Stack

Think of [Nix][nix] like [Stack][stack], but with a much broader scope.

  [nix]: https://nixos.org/nix/
  [stack]: https://docs.haskellstack.org/en/stable/README/

Stack is there to manage your project's dependencies, and help you run their
programs and compile stuff with them, but only if the dependencies are Haskell
packages.

Nix dependencies can be... kinda any software at all.

Nix and Stack work together nicely. When you enable [Nix integration for
Stack][integration], you can add any software as a dependency of your Stack
project.

  [integration]: https://github.com/commercialhaskell/stack/blob/master/doc/nix_integration.md

So if you want to, for example, add a [SASS][sass] compilation step to your
[Hakyll][hakyll] project, your coworkers don't have to do any work to get the
SASS compiler. Stack will just install it for them.

  [sass]: http://sass-lang.com/
  [hakyll]: https://jaspervdj.be/hakyll/

Like Stack keeps a cache of all the stuff it has installed in `~/.stack` in your
home directory, Nix keeps all of its stuff in `/nix/store`. (It's not in your
home directory, but at the root of the filesystem — for *technical reasons* —
which is why you are prompted to sudo when you install it.)

## `nix-shell` is like `stack exec`

Another comparison between Stack and Nix: You know how you can use Stack
(outside the context of any project) to run executables from Haskell packages?

```
> stack exec --package pandoc -- pandoc --version
pandoc 1.19.2.1
```

`nix-shell` is like `stack exec`.

So take for example... Let's assume you don't have NPM installed. And let's say
you don't really want to install it permanently or whatever because you're
probably not going to need it again, but you just want to run it for some
one-off command.

```
> nix-shell -p npm --run 'npm --version'
4.1.1
```

`-p npm` is like `--package pandoc`; it tells Nix what packages you want
available in the sandbox you're going to run a command in. And then `npm
--version` is the command you want to run. So that'll download NPM and run `npm
--version`, without actually “installing” NPM — without modifying your system
visibly at all.

## `shell.nix` is like `stack.yaml`

So then, also like Stack, you can use it in a sort of standalone manner like
that, or you can have a config file for a project. Here's one I'm using right
now:

```nix
{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "joyofhaskell.com";
  inherit ghc;
  buildInputs = with pkgs; [ sassc zlib ];
  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
```

This file is called `shell.nix` and it sits in the root of a Hakyll project
repository alongside `stack.yaml`.

The most important line here is the one with our dependencies:

```nix
buildInputs = with pkgs; [ sassc zlib ];
```

And then at the end of our `stack.yaml` is

```yaml
nix:
  enable: true
  pure: true
  shell-file: shell.nix
```

which tells Stack to do everything that it does in the context of that Nix
configuration, which it does by using `nix-shell` under the hood.

## `nix-env` is like `apt-get`

You can also use Nix as an alternative to Ubuntu's package manager (`apt-get`)
for a lot of things if you so choose. (I'm assuming you're using Ubuntu.)

For this we use another command, `nix-env`.

The basic usage `of nix-env` is:

* `nix-env -i npm` — to install (e.g. NPM)
* `nix-env -e npm` — to uninstall
* `nix-env -q` — to list which package you have installed

You have no pressing need to use `nix-env` instead of your existing package
manager at the moment, but it will be important if you ever use [NixOS][nixos].

  [nixos]: https://nixos.org/

An interesting difference between `nix-env` and `apt-get` is that it only
affects your user, which is why you don't have to sudo to run it.
