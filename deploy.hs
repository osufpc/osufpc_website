#!/usr/bin/env stack
-- stack --resolver lts-11.7 script

{-# LANGUAGE OverloadedStrings #-}

-- | Run me with ./deploy.hs !!!

import Prelude hiding (concat)
import Turtle hiding (append)
import Turtle.Options
import Data.Text (Text,append, concat)
-- scp -r _site/* youngjef@access.engr.oregonstate.edu:/nfs/farm/groups/fpc/public_html

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

  -- now we scp the site out to public_html
  shell ("scp " `append` concat ["-r "
                                , folder
                                , " "
                                , format fp onid `append` "@" `append` server
                                ]) empty
