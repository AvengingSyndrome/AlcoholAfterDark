module Handler.Offer where

import Import
import Yesod.Form.Nic (nicHtmlField)
import AahCommon
getOfferR :: OfferId -> Handler Html
getOfferR offerId = do
    offer <- runDB $ get404 offerId
    proposal <- runDB $ get404 $ offerProposal offer
    let sale = proposal
    comments <- runDB $ selectList [CommentOffer ==. offerId] [Asc CommentPostTime]
    authid <- maybeAuthId 
    case authid of 
        Just aid -> do
          (commentWidget, enctype) <- generateFormPost $ makeCommentForm aid offerId
          (offerAcceptWidget, oawEnctype) <- generateFormPost $ acceptOffer (offerProposal offer) offerId 
          defaultLayout $ do
             setTitle $ toHtml $ proposalTitle proposal
             $(widgetFile "sale")
             $(widgetFile "offer")
             $(widgetFile "comments")
             if (aid == offerUser offer ||  aid == proposalPosterUser proposal) 
                then $(widgetFile "commentform")
                else mempty
             if (aid == proposalPosterUser proposal) 
                then $(widgetFile "acceptOffer")
                else mempty 
        Nothing -> 
         defaultLayout $ do
             setTitle $ toHtml $ proposalTitle sale
             $(widgetFile "sale")
             $(widgetFile "comments")
postOfferR :: OfferId -> Handler Html
postOfferR offerId = do
    aid <- requireAuthId
    offer <- runDB $ get404 offerId
    proposal <- runDB $ get404 $ offerProposal offer
    if (aid == offerUser offer || aid == proposalPosterUser proposal) 
      then do
       ((res,_),_) <- runFormPost $  makeCommentForm aid offerId
       case res of 
         FormSuccess comment -> do
            _ <- runDB $ insert comment   
            setMessage $ "Posted!"
            getOfferR offerId
         _ ->  do 
            getOfferR offerId
      else do 
       setMessage "You can't post to this page!"
       getOfferR offerId

makeCommentForm :: Key User -> Key Offer -> Form Comment
makeCommentForm uid offer = renderDivs $ Comment
    <$> lift (liftIO getCurrentTime)
    <*> pure uid 
    <*> pure offer
    <*> areq nicHtmlField "Content" Nothing
