{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}
import Yesod
import Yesod.Form.Jquery
import Data.Time (Day)
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))

data Synopsis = Synopsis

mkYesod "Synopsis" [parseRoutes|
/ RootR GET
/person PersonR POST
|]

instance Yesod Synopsis

instance RenderMessage Synopsis FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery Synopsis

-- The datatype we wish to receive from the form
data Person = Person
    { personName :: Text
    , personBirthday :: Day
    , personFavoriteColor :: Maybe Text
    , personEmail :: Text
    , personWebsite :: Maybe Text
    }
  deriving Show

-- Declare the form. The type signature is a bit intimidating, but here's the
-- overview:
--
-- * The Html parameter is used for encoding some extra information. See the
-- discussion regarding runFormGet and runFormPost below for further
-- explanation.
--
-- * We have the sub and master site types, as usual.
--
-- * FormResult can be in three states: FormMissing (no data available),
-- FormFailure (invalid data) and FormSuccess
--
-- * The Widget is the viewable form to place into the web page.
--
-- Note that the scaffolded site provides a convenient Form type synonym,
-- so that our signature could be written as:
--
-- > personForm :: Form Person
--
-- For our purposes, it's good to see the long version.
personForm :: Html -> MForm Synopsis Synopsis (FormResult Person, Widget)
personForm = renderDivs $ Person
    <$> areq textField "Name" Nothing
    <*> areq (jqueryDayField def
        { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "1900:-5" -- 1900 till five years ago
        }) "Birthday" Nothing
    <*> aopt textField "Favorite color" Nothing
    <*> areq emailField "Email address" Nothing
    <*> aopt urlField "Website" Nothing

-- The GET handler displays the form
getRootR :: Handler RepHtml
getRootR = do
    -- Generate the form to be displayed
    ((_, widget), enctype) <- generateFormPost personForm
    defaultLayout [whamlet|
<p>The widget generated contains only the contents of the form, not the form tag itself. So...
<form method=post action=@{PersonR} enctype=#{enctype}>
    ^{widget}
    <p>It also doesn't include the submit button.
    <input type=submit>
|]

-- The POST handler processes the form. If it is successful, it displays the
-- parsed person. Otherwise, it displays the form again with error messages.
postPersonR :: Handler RepHtml
postPersonR = do
    ((result, widget), enctype) <- runFormPost personForm
    case result of
        FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
        _ -> defaultLayout [whamlet|
<p>Invalid input, let's try again.
<form method=post action=@{PersonR} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|]

main :: IO ()
main = warpDebug 3000 Synopsis
