module Handler.ReportIssue where

import Import

getReportIssueR :: Handler RepHtml
getReportIssueR = defaultLayout $ do
        h2id <- lift newIdent
        setTitle "WebToInk Report Issue"
        $(widgetFile "reportIssue")
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
