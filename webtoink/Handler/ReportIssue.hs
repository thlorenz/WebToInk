module Handler.ReportIssue where

import Import

getReportIssueR :: Handler RepHtml
getReportIssueR = defaultLayout $ do
        liftIO $ logd "GET ReportIssue"
        h2id <- lift newIdent
        setTitle "WebToInk Report Issue"
        $(widgetFile "reportIssue")
