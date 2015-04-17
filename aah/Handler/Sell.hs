module Handler.Sell where

import Import
import Yesod.Form.Nic (nicHtmlField)

getSellR :: ProposalId -> Handler Html
getSellR saleId = do
    sale <- runDB $ get404 saleId
    offers <- runDB $ selectList [OfferProposal ==. saleId] [Desc OfferPostTime]
    authid <- maybeAuthId 
    case authid of 
        Just aid -> do
          (offerWidget, enctype) <- generateFormPost $ makeOfferForm sale saleId aid
          defaultLayout $ do
             setTitle $ toHtml $ proposalTitle sale
             $(widgetFile "sale")
             $(widgetFile "offers")
             $(widgetFile "offerform")
        Nothing -> 
         defaultLayout $ do
             setTitle $ toHtml $ proposalTitle sale
             $(widgetFile "sale")
             $(widgetFile "offers")

postSellR :: ProposalId -> Handler Html
postSellR saleId = do
    aid <- requireAuthId
    sale <- runDB $ get404 saleId
    ((res,_),_) <- runFormPost $  makeOfferForm sale saleId aid
    case res of
         FormSuccess offer -> do
            _ <- runDB $ insert offer   
            setMessage $ "Posted!"
            getSellR saleId
         _ ->  do 
            getSellR saleId
    

makeOfferForm :: Proposal -> Key Proposal -> Key User -> Form Offer
makeOfferForm proposal pid uid = renderDivs $ Offer
    <$> lift (liftIO getCurrentTime)
    <*> areq doubleField "Price" (Just (proposalPrice proposal))
    <*> pure uid
    <*> pure False
    <*> pure pid
    <*> areq nicHtmlField "Content" Nothing
