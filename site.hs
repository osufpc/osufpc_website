--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import qualified GHC.IO.Encoding as E

--------------------------------------------------------------------------------
main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    hakyll $ do
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

      -- | this route catches all markdown files with these extensions in the root
      -- direction (the dir with site.hs) it then converts them to .html files and
      -- will match the routes based on their filenames, so contact.markdown becomes
      -- contact.html
      -- THIS MATCH WILL CATCH AND OVERRIDE ALL TOP LEVEL MARKDOWN FILES
        match ("*.markdown" .||. "*.md") $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Archives"            `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls

      -- | the index.markdown file is captured by the route above, this one is
      -- merely setting it to the default route for the site
        match "index.html" $
          do route idRoute
             route $ setExtension "html"
             let indexCtx = constField "title" "Home" <> defaultContext
             compile $ getResourceBody
               >>= applyAsTemplate indexCtx
               >>= loadAndApplyTemplate "templates/default.html" indexCtx
               >>= relativizeUrls

        -- | this route matches the markdown files for the tutorials and creates html pages
        match "tutorials/*" $ do
            route idRoute
            route $ setExtension "html"
            compile $ pandocCompiler
                    >>= applyAsTemplate defaultContext
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
                    >>= relativizeUrls

        --match "posts/*" $ do
        --    route idRoute
        --    compile $ do
        --        posts <- recentFirst =<< loadAll "posts/*"
        --        let indexCtx =
        --                listField "posts" postCtx (return posts) `mappend`
        --                constField "title" "Home"                `mappend`
        --                defaultContext
        --        getResourceBody
        --            >>= applyAsTemplate indexCtx
        --            >>= loadAndApplyTemplate "templates/default.html" indexCtx
        --            >>= relativizeUrls

        -- | This route will construct the actual tutorials page that will list
        -- out the compiled tutorials
        match "tutorials.html" $ do
            route idRoute
            route $ setExtension "html"
            compile $ do
                tutorials <- loadAll "tutorials/*"
                let tutorialsCtx =
                        listField "tutorials" postCtx (return tutorials) `mappend`
                        defaultContext
                getResourceBody
                    >>= applyAsTemplate tutorialsCtx
                    >>= loadAndApplyTemplate "templates/default.html" tutorialsCtx
                    >>= relativizeUrls

        match "events.html" $ do
            route idRoute
            compile $ do
                events <- recentFirst =<< loadAll "events/*"
                let eventCtx =
                        listField "events" postCtx (return events) `mappend`
                        constField "title" "Events"                `mappend`
                        defaultContext
                getResourceBody
                    >>= applyAsTemplate eventCtx
                    >>= loadAndApplyTemplate "templates/default.html" eventCtx
                    >>= relativizeUrls


        match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
