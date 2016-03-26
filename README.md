# covaur [![Build Status](https://travis-ci.org/Thimoteus/covaur.svg?branch=master)](https://travis-ci.org/Thimoteus/covaur)

## What is it?

An AUR helper for Arch Linux users. It's main use is to search the AUR and get
a package's (AUR) git repo address for cloning.

## How do I use it?

People new to Arch Linux are told to understand how to manually install a package
from the AUR before using a helper (whether most people do that or go straight
to a helper they see everyone else using is a different story ... ).

For example, you might go to https://aur.archlinux.org/ and search for a package
-- let's say [`purescript-bin`](https://aur.archlinux.org/packages/purescript-bin/).
You might then download the snapshot, or copy the `Git Clone URL` and clone it
on your system. Then you'd proceed with the usual `makepkg` magic.

`covaur` cuts out the browser step. Instead, you could do `covaur --search purescript`,
confirm the package you want is called 'purescript-bin', then do
`git clone $(covaur --git-url purescript-bin)`.

## Installing

You need node.js and the `npm` binary. Then just run `npm install -g .` from inside
this folder.
