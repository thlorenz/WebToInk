> {-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
> import Yesod
> import Data.Monoid (mempty)

Like the blog example, we'll define some data first.

> data Page = Page
>   { pageName :: String
>   , pageSlug :: String
>   , pageContent :: String
>   }

> loadPages :: IO [Page]
> loadPages = return
>   [ Page "Page 1" "page-1" "My first page"
>   , Page "Page 2" "page-2" "My second page"
>   , Page "Page 3" "page-3" "My third page"
>   ]

> data Ajax = Ajax
>   { ajaxPages :: [Page]
>   , ajaxStatic :: Static
>   }

> type Handler = GHandler Ajax Ajax

Next we'll generate a function for each file in our static folder. This way,
 we get a compiler warning when trying to using a file which does not exist.

> staticFiles "static/yesod/ajax"

Now the routes; we'll have a homepage, a pattern for the pages, and use a
 static subsite for the Javascript and CSS files.

> mkYesod "Ajax" [$parseRoutes|
> /                  HomeR   GET
> /page/#String      PageR   GET
> /static            StaticR Static ajaxStatic
> |]

That third line there is the syntax for a subsite: Static is the datatype
 for the subsite argument; siteStatic returns the site itself (parse, render
 and dispatch functions); and ajaxStatic gets the subsite argument from the
 master argument.
Now, we'll define the Yesod instance. We'll still use a dummy approot
 value, but we're also going to define a default layout.

> instance Yesod Ajax where
>   approot _ = ""
>   defaultLayout widget = do
>   Ajax pages _ <- getYesod
>   content <- widgetToPageContent widget
>   hamletToRepHtml [$hamlet|
> \<!DOCTYPE html>
> 
> <html>
>   <head>
>     <title>#{pageTitle content}
>     <link rel="stylesheet" href="@{StaticR style_css}">
>     <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js">
>     <script src="@{StaticR script_js}">
>     \^{pageHead content}
>   <body>
>     <ul id="navbar">
>       $forall page <- pages
>         <li>
>           <a href="@{PageR (pageSlug page)}">#{pageName page}
>     <div id="content">
>       \^{pageBody content}
> |]

> getHomeR :: Handler ()
> getHomeR = do
>   Ajax pages _ <- getYesod
>   let first = head pages
>   redirect RedirectTemporary $ PageR $ pageSlug first

And now the cool part: a handler that returns either HTML or JSON data,
 depending on the request headers.

> getPageR :: String -> Handler RepHtmlJson
> getPageR slug = do
>   Ajax pages _ <- getYesod
>   case filter (\e -> pageSlug e == slug) pages of
>       [] -> notFound
>       page:_ -> defaultLayoutJson (do
>           setTitle $ string $ pageName page
>           addHamlet $ html page
>           ) (json page)
>  where
>   html page = [$hamlet|
> <h1>#{pageName page}
> <article>#{pageContent page}
> |]
>   json page = jsonMap
>       [ ("name", jsonScalar $ pageName page)
>       , ("content", jsonScalar $ pageContent page)
>       ]

> main :: IO ()
> main = do
>   pages <- loadPages
>   let s = static "static/yesod/ajax"
>   warpDebug 3000 $ Ajax pages s
