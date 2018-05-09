#!/usr/bin/env stack
-- stack --resolver lts-11.7 script

{-# LANGUAGE OverloadedStrings #-}

-- | Run me with ./deploy.hs !!!
-- | Or compile me with  stack ghc -- -O2 -threaded -dynamic deploy.hs

import Turtle (shell, empty, format, fp, argPath, echo)
import Turtle.Options (options)
import Data.Text (Text,append,concat)

parser = argPath "onid" "Your onid username"

server :: Text
server = "access.engr.oregonstate.edu:/nfs/farm/groups/fpc/public_html"

folder :: Text
folder = "_site/*"


main = do
  onid <- options "A simple wrapper around scp to deploy easily" parser

  -- I'm just hardcoding this, turtle does have support for this but I don't
  -- expect these to change
  shell "find _site/ -type d -exec chmod 755 {} \\;" empty
  shell "find _site/ -type f -exec chmod 644 {} \\;" empty

  -- now we rsync the site out to public_html
  shell ("rsync " `append` mconcat ["-av "
                                  , folder
                                  , " "
                                  , format fp onid `append` "@" `append` server
                                  ]) empty

  echo "MAKE SURE YOU COMMIT YOUR CHANGES TO THE REPO!!!"
