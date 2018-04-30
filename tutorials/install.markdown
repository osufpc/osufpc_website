---
title: Installing Haskell
date: 2018-04-30
page: install.html
author: Jeffrey Young
---
### Installing Haskell, a long story
Haskell is a research language that was designed by committee. As such it is
only recently getting the support that you may expect in similarly old languages
such as python or even ruby. Installation is going to all come down to your
platform but I'll go through the most common ones here.

#### An Overview of Haskell Projects
Haskell does not have a proper package manager such as Maven, Ant, Leiningen
(Clojure), cargo (Rust), bundler (Ruby) etc. What it has are build tools known
as cabal and stack. For non-legacy projects I prefer to use stack because it is
focused on *reproducible builds*. For this tutorial I just wanted you to be
aware of some Haskell vernacular you will encounter on the internet.

#### Windows
The haskell story on windows has gotten significantly better of the last few
years. Simply download the [haskell
platform](https://www.haskell.org/platform/windows.html) and follow the
instructions. You'll then want to download the `stack` tool
[here](https://docs.haskellstack.org/en/stable/README/). Now to make a new
project from a terminal simply go to some folder and run the following:

```
stack new my-project      // this will create your project's root folder
cd my-project             // change directories to that folder
stack setup               // This will take care of downloading the glorious haskell compiler
stack build               // This will build the bare bones project to an executable
stack exec my-project-exe // This will execute that executable (i.e. ./my-project-exe)
```

If you run into any issues then ask questions during the next meeting or on IRC.
You can also check that you can run the haskell interpreter `ghci` with:

```
stack ghci
```

#### Mac
You can install Haskell on Mac similarly to the Windows install i.e. download
the [haskell platform](https://www.haskell.org/platform/windows.html), and then
following the instructions below for stack. Installing with homebrew IMHO is
easier and easier to maintain simply open a terminal and run homebrew to install
`ghc` and `cabal-install` like so:

```
brew install ghc cabal-install
```

homebrew may have older versions of the glorious haskell compiler which you can
check via:

```
brew info ghc cabal-install
```

Similarly, to install `stack` on Mac just use homebrew like:

```
brew install haskell-stack
```

Now verify that you can create a project or run `ghci` as shown in the Windows
section above

#### Linux
This will depend on your distro for Ubuntu, Debian and other popular distros
chances are that haskell and stack are in your package manager already, so check
your particular distros' distribution. If you are on NixOS or Arch Linux see
below:

##### NixOS
In your `configration.nix` file just add this snippet or isolate it to a separte
`*.nix` file if you want:

```
## Monolithic system package description
environment.systemPackages = with pkgs; [ # haskell dependencies
                                          ghc
                                          stack
                                          cabal-install
                                        ];
```

Now rebuild

```
sudo nixos-rebuild switch
```

and you should be able to run a `stack ghci` as described above from the command
line.

##### Arch
As always check the [arch wiki](https://wiki.archlinux.org/index.php/haskell) I
would recommend letting `stack` handle most of your system wide dependencies to
avoid having to download all your packages through pacman. Similarly instead of
downloading and building stack as the wiki suggests, you'll be better served by
downloading `stack-static` or `stack-bin` from the AUR.
