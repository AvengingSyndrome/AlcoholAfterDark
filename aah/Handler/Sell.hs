module Handler.Sell where

import Import

getSellR :: ProposalId -> Handler Html
getSellR saleId = do
    sale <- runDB $ get404 saleId
    defaultLayout $ do
        setTitle $ toHtml $ proposalTitle sale
        $(widgetFile "sale")
       -- $(widgetFile "offers")
       -- $(widgetFile "makeoffer")

postSellR :: ProposalId -> Handler Html
postSellR saleId = error "Not yet implemented: getSellR"
