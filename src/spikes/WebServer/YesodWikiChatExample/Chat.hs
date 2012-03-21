
-- @Chat.hs
{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts
  #-}
-- | This modules defines a subsite that allows you to insert a chat box on
-- any page of your site. It uses eventsource for sending the messages from
-- the server to the browser.
module Chat where

import Yesod
import Control.Concurrent.Chan (Chan, dupChan, writeChan)
import Data.Text (Text)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Language.Haskell.TH.Syntax (Type (VarT), Pred (ClassP), mkName)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.Monoid (mappend)

-- | Our subsite foundation. We keep a channel of events that all connections
-- will share.
data Chat = Chat (Chan ServerEvent)

-- | We need to know how to check if a user is logged in and how to get
-- his/her username (for printing messages).
class (Yesod master, RenderMessage master FormMessage)
        => YesodChat master where
    getUserName :: GHandler sub master Text
    isLoggedIn :: GHandler sub master Bool

-- Now we set up our subsite. The first argument is the subsite, very similar
-- to how we've used mkYesod in the past. The second argument is specific to
-- subsites. What it means here is "the master site must be an instance of
-- YesodChat".
--
-- We define two routes: a route for sending messages from the client to the
-- server, and one for opening up the event stream to receive messages from
-- the server.
mkYesodSub "Chat"
    [ ClassP ''YesodChat [VarT $ mkName "master"]
    ] [parseRoutes|
/send SendR POST
/recv ReceiveR GET
|]

-- | Get a message from the user and send it to all listeners.
postSendR :: YesodChat master => GHandler Chat master ()
postSendR = do
    from <- getUserName

    -- Note that we're using GET parameters for simplicity of the Ajax code.
    -- This could easily be switched to POST. Nonetheless, our overall
    -- approach is still RESTful since this route can only be accessed via a
    -- POST request.
    body <- runInputGet $ ireq textField "message"

    -- Get the channel
    Chat chan <- getYesodSub

    -- Send an event to all listeners with the user's name and message.
    liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $
        fromText from `mappend` fromText ": " `mappend` fromText body

-- | Send an eventstream response with all messages streamed in.
getReceiveR :: GHandler Chat master ()
getReceiveR = do
    -- First we get the main channel
    Chat chan0 <- getYesodSub

    -- We duplicated the channel, which allows us to create broadcast
    -- channels.
    chan <- liftIO $ dupChan chan0

    -- Now we use the event source API. eventSourceAppChan takes two parameters:
    -- the channel of events to read from, and the WAI request. It returns a
    -- WAI response, which we can return with sendWaiResponse.
    req <- waiRequest
    res <- lift $ eventSourceAppChan chan req
    sendWaiResponse res

-- | Provide a widget that the master site can embed on any page.
chatWidget :: YesodChat master
           => (Route Chat -> Route master)
           -> GWidget sub master ()
-- This toMaster argument tells us how to convert a Route Chat into a master
-- route. You might think this is redundant information, but taking this
-- approach means we can have multiple chat subsites in a single site.
chatWidget toMaster = do
    -- Get some unique identifiers to help in creating our HTML/CSS. Remember,
    -- we have no idea what the master site's HTML will look like, so we
    -- should not assume we can make up identifiers that won't be reused.
    -- Also, it's possible that multiple chatWidgets could be embedded in the
    -- same page.
    chat <- lift newIdent   -- the containing div
    output <- lift newIdent -- the box containing the messages
    input <- lift newIdent  -- input field from the user

    ili <- lift isLoggedIn  -- check if we're already logged in
    if ili
        then do
            -- Logged in: show the widget
            [whamlet|
<div ##{chat}>
    <h2>Chat
    <div ##{output}>
    <input ##{input} type=text placeholder="Enter Message">
|]
            -- Just some CSS
            toWidget [lucius|
##{chat} {
    position: absolute;
    top: 2em;
    right: 2em;
}
##{output} {
    width: 200px;
    height: 300px;
    border: 1px solid #999;
    overflow: auto;
}
|]
            -- And now that Javascript
            toWidgetBody [julius|
// Set up the receiving end
var output = document.getElementById("#{output}");
var src = new EventSource("@{toMaster ReceiveR}");
src.onmessage = function(msg) {
    // This function will be called for each new message.
    var p = document.createElement("p");
    p.appendChild(document.createTextNode(msg.data));
    output.appendChild(p);

    // And now scroll down within the output div so the most recent message
    // is displayed.
    output.scrollTop = output.scrollHeight;
};

// Set up the sending end: send a message via Ajax whenever the user hits
// enter.
var input = document.getElementById("#{input}");
input.onkeyup = function(event) {
    var keycode = (event.keyCode ? event.keyCode : event.which);
    if (keycode == '13') {
        var xhr = new XMLHttpRequest();
        var val = input.value;
        input.value = "";
        var params = "?message=" + encodeURI(val);
        xhr.open("POST", "@{toMaster SendR}" + params);
        xhr.send(null);
    }
}
|]
        else do
            -- User isn't logged in, give a not-logged-in message.
            master <- lift getYesod
            [whamlet|
<p>
    You must be #
    $maybe ar <- authRoute master
        <a href=@{ar}>logged in
    $nothing
        logged in
    \ to chat.
|]
