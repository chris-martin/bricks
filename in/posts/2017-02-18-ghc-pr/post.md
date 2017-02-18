--------------------------------------------------------------------------------
title:    Submitting a pull request to GHC
date:     2017 Feb 18
slug:     phabricator-ghc-pull-request
abstract: Contributing to Haskell isn't scary -
          How to submit a PR to GHC using Phabricator.
--------------------------------------------------------------------------------

When you find things incomplete or unclear in Haskell's documentation, please
fix them! I've submitted a handful of pull requests so far to make tweaks to the
API docs. GHC seemed intimidating at first, but I've found the maintainers to be
quite friendly.

GHC manages its code on a GitHub-like website called Phabricator, and I'm
writing this short guide because Phabricator is unfamiliar to most of us.

## Get the repo

Clone the GHC repository and its submodules:

```bash
git clone --recursive git://git.haskell.org/ghc.git
```

## Create an account

Before submitting pull requests, you'll need to create an account at
[phabricator.haskell.org](https://phabricator.haskell.org/) and upload your SSH
public key.

## Submit a PR

Once you've committed your changes to some local branch, use Arcanist to submit
the PR. Arcanist will run some style checks that require Python, so you'll need
that installed as well.

You only need to learn one Arcanist command: `arc diff`.

Here's a command to submit a PR to the `master` branch using
[Nix](https://nixos.org/nix/):

```bash
nix-shell -p arcanist python3 'arc diff master'
```

Once your PR has been uploaded successfully, Arcanist will print a URL where
you can go to look at the discussion on it.
