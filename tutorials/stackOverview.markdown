---
title: "Stack: A Haskell build tool Overview"
author: Jeffrey Young
date: 2018-05-01
page: stackOverview.html
---

### Motivation and Precursor
Stack is a relatively new addition to the haskell ecosystem appearing in 2015.
Before stack, everyone who developed in haskell used the
[cabal](https://www.haskell.org/cabal/) build tool to manage dependencies and
projects, which quickly becomes untenable unless you are an expert in `cabal`.
Hence `stack` was born to avoid the well known phenomena of cabal hell (here are
some links for your schadenfreude:
[one](https://www.well-typed.com/blog/2014/09/how-we-might-abolish-cabal-hell-part-1/),
[two](https://www.yesodweb.com/blog/2012/11/solving-cabal-hell),
[three](https://wiki.haskell.org/Cabal/Survival))

#### An Overview of Haskell Projects in Stack
New projects in stack are created from the command line with the `stack new`
command like so:

```
stack new my-awesome-project
```

This will create a new sub-directory called `my-awesome-project` for you and
build out the directory structure for a typical stack project. I won't go
through everything in detail but just give you a taste of project organization.
For example here is the structure of my research projects:

```
haskell/
├── app
│   └── Main.hs
├── bench
│   └── Baselines.hs
├── LICENSE
├── package.yaml
├── README.md
├── Setup.hs
├── src
│   ├── Run.hs
│   ├── SAT.hs
│   ├── Utils.hs
│   ├── V.hs
│   └── VProp.hs
├── stack.yaml
├── test
│   ├── Gen.hs
│   └── Spec.hs
└── vsat.cabal
```

`app` file should contain any files that are related to executables. `src`
should include your haskell source files. `test` should include any test files
and `bench` should include benchmarking files.

There are only two files that control stack's behavior they are `package.yaml`
and `stack.yaml`. When you run `stack build` both files are parsed, stack will
resolve any missing dependencies, download the correct version of `ghc`, and
write a `my-awesome-app.cabal` file. This the `.cabal` file is what is actually
sent to haskell's compiler `ghc` underneath the hood. Thus `stack` sits **on
top** of `cabal`. Now let's look at these two files in detail:

#### The stack.yaml file
The `stack.yaml` file for that research project looks like this:

```
flags: {}
packages:
# system-ghc: false
extra-deps: []

resolver: lts-10.10
build:
  library-profiling: true
  executable-profiling: true
```

Depending on the template used to build your project your's might look
different. I have stripped out a lot of non-essential stuff for the sake of this
tutorial. The most important part of the `stack.yaml` file is the `resolver:`
field. This determines which version of `ghc` will be downloaded and sandboxed
with your project. This is important because the haskell package server
`hackage` is littered with successful, failed or experimental phd libraries,
some of which are rarely updated. So when adding many dependencies to your
project it can become extremely easy to mess up the dependency environment.
Stack resolves this issue by declaring a ghc version that is long-term-supported
on `stackage`, their version of `hackage`, which details any dependencies and
which versions of ghc are allowed for a given package. This information is what
makes `stack` create more reproducible builds than `cabal`. You'll also observe
that I've turned on exectuable and library profiling for my project because I'm
interested in its memory and timing performance. Yes it is that easy.

#### The package.yaml file
This is most likely the file you will be interacting with more. Here is what my
`package.yaml` file looks like for my research project:

```
name:                vsat
version:             0.1.0.0
github:              "doyougnu/vsat"
license:             BSD3
author:              "Jeffrey Young"
maintainer:          "youngjef@oregonstate.edu"
copyright:           "2018 Author name here"

extra-source-files:
- README.md

# Metadata used when publishing your package
# synopsis:            Short description of your package
# category:            Web

# To avoid duplicated efforts in documentation and dealing with the
# complications of embedding Haddock markup inside cabal files, it is
# common to point users to the README.md file.
description:         Please see the README on Github at <https://github.com/githubuser/my-app#readme>

dependencies:
- base >= 4.7 && < 5
- containers >= 0.5
- mtl
- foldl
- bifunctors
- QuickCheck
- deepseq
- sbv
- cassava
- bytestring
- criterion
- text
- vector

library:
  source-dirs: src

default-extensions:
  - NamedFieldPuns
  - OverloadedStrings
  - FlexibleContexts
  - DeriveFunctor
  - DeriveGeneric
  - BangPatterns
  - GeneralizedNewtypeDeriving
  - DeriveTraversable
  - FlexibleInstances
  - DeriveDataTypeable

executables:
  vsat:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    - -Wall
    - -fno-warn-orphans
    dependencies:
    - vsat

benchmarks:
  vsat-bench:
    main:                Baselines.hs
    source-dirs:
      - bench
      - src
    ghc-options:
    - -O2
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
      - criterion


tests:
  vsat-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - vsat
```

The `package.yaml` file is a recent addition to `stack v1.6`. It is a bundling
of stack with the `HPack` utility. This file essentially describes the `.cabal`
file but has many benefits over manually manipulating the `.cabal` file of your
project. The first and most important is that it allows you to declare
dependencies once, instead of many times. This is shown above via the top-level
`dependencies:` tag. If we were manually configuring with the `.cabal` file I
would be forced to repeat these dependencies for every section `executables,
tests, benchmarks`. The second major benefit to using `package.yaml` and `HPack`
is that you do not need to explicitly expose each module by hand. In this project
I have five modules: `VProp.hs`, `Run.hs`, `Sat.hs`, `Utils.hs`, and `V.hs`. If
I were manually configuring I would be forced to explicitly list these all as
`exposed-modules:` in my `.cabal` file. By using `HPack` and the `package.yaml`
file I can simply declare them all as exposed via this line:

```
library:
  source-dirs: src
```

and `HPack` will take care of the rest. To see the difference, here is the
auto-generated `.cabal` file for this project:

```
-- This file has been generated from package.yaml by hpack version 0.28.2.
--
-- see: https://github.com/sol/hpack
--
-- hash: 101019861e88b49f36eb5b15812738c1cb2afd3ee9da4a87b87322d7f3f50bdc

name:           vsat
version:        0.1.0.0
description:    Please see the README on Github at <https://github.com/githubuser/my-app#readme>
homepage:       https://github.com/doyougnu/vsat#readme
bug-reports:    https://github.com/doyougnu/vsat/issues
author:         Jeffrey Young
maintainer:     youngjef@oregonstate.edu
copyright:      2018 Author name here
license:        BSD3
license-file:   LICENSE
build-type:     Simple
cabal-version:  >= 1.10
extra-source-files:
    README.md

source-repository head
  type: git
  location: https://github.com/doyougnu/vsat

library
  exposed-modules:
      Run
      SAT
      Utils
      V
      VProp
  other-modules:
      Paths_vsat
  hs-source-dirs:
      src
  default-extensions: NamedFieldPuns OverloadedStrings FlexibleContexts DeriveFunctor DeriveGeneric BangPatterns GeneralizedNewtypeDeriving DeriveTraversable FlexibleInstances DeriveDataTypeable
  build-depends:
      QuickCheck
    , base >=4.7 && <5
    , bifunctors
    , bytestring
    , cassava
    , containers >=0.5
    , criterion
    , deepseq
    , foldl
    , mtl
    , sbv
    , text
    , vector
  default-language: Haskell2010

executable vsat
  main-is: Main.hs
  other-modules:
      Paths_vsat
  hs-source-dirs:
      app
  default-extensions: NamedFieldPuns OverloadedStrings FlexibleContexts DeriveFunctor DeriveGeneric BangPatterns GeneralizedNewtypeDeriving DeriveTraversable FlexibleInstances DeriveDataTypeable
  ghc-options: -threaded -rtsopts -with-rtsopts=-N -Wall -fno-warn-orphans
  build-depends:
      QuickCheck
    , base >=4.7 && <5
    , bifunctors
    , bytestring
    , cassava
    , containers >=0.5
    , criterion
    , deepseq
    , foldl
    , mtl
    , sbv
    , text
    , vector
    , vsat
  default-language: Haskell2010

test-suite vsat-test
  type: exitcode-stdio-1.0
  main-is: Spec.hs
  other-modules:
      Gen
      Paths_vsat
  hs-source-dirs:
      test
  default-extensions: NamedFieldPuns OverloadedStrings FlexibleContexts DeriveFunctor DeriveGeneric BangPatterns GeneralizedNewtypeDeriving DeriveTraversable FlexibleInstances DeriveDataTypeable
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      QuickCheck
    , base >=4.7 && <5
    , bifunctors
    , bytestring
    , cassava
    , containers >=0.5
    , criterion
    , deepseq
    , foldl
    , mtl
    , sbv
    , text
    , vector
    , vsat
  default-language: Haskell2010

benchmark vsat-bench
  type: exitcode-stdio-1.0
  main-is: Baselines.hs
  other-modules:
      Run
      SAT
      Utils
      V
      VProp
      Paths_vsat
  hs-source-dirs:
      bench
      src
  default-extensions: NamedFieldPuns OverloadedStrings FlexibleContexts DeriveFunctor DeriveGeneric BangPatterns GeneralizedNewtypeDeriving DeriveTraversable FlexibleInstances DeriveDataTypeable
  ghc-options: -O2 -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      QuickCheck
    , base >=4.7 && <5
    , bifunctors
    , bytestring
    , cassava
    , containers >=0.5
    , criterion
    , deepseq
    , foldl
    , mtl
    , sbv
    , text
    , vector
  default-language: Haskell2010
```

#### Other stuff
`stack` is a deep tool that can be used for tons of useful things. If you are
looking for something I have not covered here then I suggest you check out
[their offical docs](https://docs.haskellstack.org/en/stable/README/) or go chat
with them on the `#haskell-stack` irc channel.
