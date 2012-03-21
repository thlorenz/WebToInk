{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts
  #-}
import Yesod
import Yesod.Auth
import Yesod.Auth.Dummy (authDummy)
import Chat
import Control.Concurrent.Chan (Chan, newChan)
import Network.Wai.Handler.Warp (run)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.IORef as I
import qualified Data.Map as Map
-- import Text.Markdown (markdown, def)

-- | Our foundation type has both the chat subsite and a mutable reference to
-- a map of all our wiki contents. Note that the key is a list of Texts, since
-- a wiki can have an arbitrary hierarchy.
--
-- In a real application, we would want to store this information in a
-- database of some sort.
data Wiki = Wiki
    { getChat :: Chat
    , wikiContent :: I.IORef (Map.Map [Text] Text)
    }

-- Set up our routes as usual.
mkYesod "Wiki" [parseRoutes|
/ RootR GET                 -- the homepage
/wiki/*Texts WikiR GET POST -- note the multipiece for the wiki hierarchy
/chat ChatR Chat getChat    -- the chat subsite
/auth AuthR Auth getAuth    -- the auth subsite
|]

instance Yesod Wiki where
    authRoute _ = Just $ AuthR LoginR -- get a working login link

    -- Our custom defaultLayout will add the chat widget to every page.
    -- We'll also add login and logout links to the top.
    defaultLayout widget = do
        pc <- widgetToPageContent $ widget >> chatWidget ChatR
        mmsg <- getMessage
        hamletToRepHtml [hamlet|
$doctype 5
<html>
    <head>
        <title>#{pageTitle pc}
        ^{pageHead pc}
    <body>
        $maybe msg <- mmsg
            <div .message>#{msg}
        <nav>
            <a href=@{AuthR LoginR}>Login
            \ | #
            <a href=@{AuthR LogoutR}>Logout
        ^{pageBody pc}
|]

-- Fairly standard YesodAuth instance. We'll use the dummy plugin so that you
-- can create any name you want, and store the login name as the AuthId.
instance YesodAuth Wiki where
    type AuthId Wiki = Text
    authPlugins _ = [authDummy]
    loginDest _ = RootR
    logoutDest _ = RootR
    getAuthId = return . Just . credsIdent
    authHttpManager = error "authHttpManager" -- not used by authDummy

-- Just implement authentication based on our yesod-auth usage.
instance YesodChat Wiki where
    getUserName = requireAuthId
    isLoggedIn = do
        ma <- maybeAuthId
        return $ maybe False (const True) ma

instance RenderMessage Wiki FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Nothing special here, just giving a link to the root of the wiki.
getRootR :: Handler RepHtml
getRootR = defaultLayout [whamlet|
<p>Welcome to the Wiki!
<p>
    <a href=@{wikiRoot}>Wiki root
|]
  where
    wikiRoot = WikiR []

-- A form for getting wiki content
wikiForm mtext = renderDivs $ areq textareaField "Page body" mtext

-- Show a wiki page and an edit form
getWikiR :: [Text] -> Handler RepHtml
getWikiR page = do
    -- Get the reference to the contents map
    icontent <- fmap wikiContent getYesod

    -- And read the map from inside the reference
    content <- liftIO $ I.readIORef icontent

    -- Lookup the contents of the current page, if available
    let mtext = Map.lookup page content

    -- Generate a form with the current contents as the default value.
    -- Note that we use the Textarea wrapper to get a <textarea>.
    ((_, form), _) <- generateFormPost $ wikiForm $ fmap Textarea mtext
    defaultLayout $ do
        case mtext of
            -- We're treating the input as markdown. The markdown package
            -- automatically handles XSS protection for us.
            -- Just text -> toWidget $ markdown def $ TL.fromStrict text
            Just text -> [whamlet|<p>Page existed|]
            Nothing -> [whamlet|<p>Page does not yet exist|]
        [whamlet|
<h2>Edit page
<form method=post>
    ^{form}
    <div>
        <input type=submit>
|]

-- Get a submitted wiki page and updated the contents.
postWikiR :: [Text] -> Handler RepHtml
postWikiR page = do
    icontent <- fmap wikiContent getYesod
    content <- liftIO $ I.readIORef icontent
    let mtext = Map.lookup page content
    ((res, form), _) <- runFormPost $ wikiForm $ fmap Textarea mtext
    case res of
        FormSuccess (Textarea t) -> do
            liftIO $ I.atomicModifyIORef icontent $
                \m -> (Map.insert page t m, ())
            setMessage "Page updated"
            redirect $ WikiR page
        _ -> defaultLayout [whamlet|
<form method=post>
    ^{form}
    <div>
        <input type=submit>
|]

main :: IO ()
main = do
    -- Create our server event channel
    chan <- newChan

    -- Initially have a blank database of wiki pages
    icontent <- I.newIORef Map.empty

    -- Run our app
    warpDebug 3000 $ Wiki (Chat chan) icontent
