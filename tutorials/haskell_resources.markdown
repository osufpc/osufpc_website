---
title: Haskell Resources
date: 2018-05-09
page: haskell_resources.html
author: Jeffrey Young
---
### Context
This document is meant to be a one stop shop for getting help with Haskell. It
will simply be a link dump based on relative skill level.

#### Beginner Haskell
- [Learn You a Haskell (LYAH)](http://learnyouahaskell.com/chapters)
    - Often cited as the recommended beginner's guide and for great reason. You
    really cannot do better than this book to start learning Haskell. It'll take
    you from absolute beginner to understanding Monads and Zippers i.e.
    intermediate Haskeller.
- [Real World Haskell](http://book.realworldhaskell.org/)
    - Made by some pretty famous Haskeller's this book will is an absolute
    compendium on using Haskell for real projects. Although it is somewhat out
    of date now chapters 1 - 15 are still great and worth reading if you're a
    beginner. I would suggest this after reading through LYAH.
- [CIS 194](https://www.cis.upenn.edu/~cis194/spring13/)
    - The official undergraduate Haskell course taught by Brent Yorgey at Penn
    State. This is a great introduction to Haskell course that brings you from
    absolute zero to Monads. Make sure you do the exercises!

#### Intermediate Haskell
- [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/)
    - Another famous Haskeller's compedium of information. This is a great website
    to check for information on different types of Monads, different GHC
    extensions, and other things you'll hear in the Haskell community e.g.
    `mtl`, `MonadPlus`, `RWST Monad`, `Existential Types`, `GADTS`,
    `QuickCheck`, `SmallCheck` etc. It also includes a ton of practical advice
    on what to avoid in the `Prelude`, what extensions are safe and other stuff
    like that.

- [A Gentle Introduction to Haskell](https://www.haskell.org/tutorial/)
    - A great read if you are past chapters 10 and 11 in LYAH. Also famous for
      not being so gentle so make sure you know some of what LYAH teaches first!

- [Real World Haskell](http://book.realworldhaskell.org/)
    - The rest of RWH. At some point you'll want to understand how to profile your
    program in GHC, do some networking tasks, get familiar with the STM in
    haskell etc. Even though the book is recently out of date these chapters
    will still give you the context to understand the current libraries.

#### Advanced Haskell
- [School of Haskell](https://www.schoolofhaskell.com/)
    - I don't want to sell the school of Haskell short, there are plenty of useful
    beginner and intermediate resources here, but if you're looking for "What is
    the best way to do streaming computations" (the conduit library), or how to
    I lazily stream data, in a compositional way (the pipes library), then
    school of Haskell is what you are looking for.

#### Miscellaneous
- [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
    - Run by the aforementioned Brent Yorgey, this is a one stop shop for
    information on type theoretic structures. If you are wondering what a
    functor is, maybe what is it's relationship with applicative functors and
    Monads, then this is what you want. Its full of useful information on
    monoids, semigroups, the foldable typeclass, you name it and it's in here.
    I'd recommend this as a good resource for anyone who has gotten past chapter
    10 or 11 in LYAH.

- [Haskell IRC](https://wiki.haskell.org/IRC_channel)
    - Seriously, Haskell has a much higher skill ceiling than any other language
    you've touched. If you find yourself struggling, then reach out and ask the
    community for help. They are very welcoming and will typically respond
    quickly.

- [Haskell Wiki](https://wiki.haskell.org/Learning_Haskell)
    - The Haskell wiki will have articles on pretty much anything you want to
    learn, including Haskell itself. But make sure you read the articles
    carefully and slowly, or else you may miss some nuance. I'm dropping this
    link here because it lists several other resources, including text books,
    wiki-books and the various Haskell subreddits like `r/haskell` and
    `r/haskellquestions`.
