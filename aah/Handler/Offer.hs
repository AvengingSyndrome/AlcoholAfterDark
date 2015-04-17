module Handler.Offer where

import Import
import Yesod.Form.Nic (nicHtmlField)

getOfferR :: OfferId -> Handler Html
getOfferR offerId = do
    offer <- runDB $ get404 offerId
    proposal <- runDB $ get404 $ offerProposal offer
    let sale = proposal
    comments <- runDB $ selectList [CommentOffer ==. offerId] [Desc CommentPostTime]
    authid <- maybeAuthId 
    case authid of 
        Just aid -> do
          (commentWidget, enctype) <- generateFormPost $ makeCommentForm aid offerId
          defaultLayout $ do
             setTitle $ toHtml $ proposalTitle proposal
             $(widgetFile "sale")
             $(widgetFile "offer")
             $(widgetFile "comments")
             if (aid == offerUser offer || aid == proposalPosterUser proposal) 
		then $(widgetFile "commentform")
                else mempty
             
        Nothing -> 
         defaultLayout $ do
             setTitle $ toHtml $ proposalTitle sale
             $(widgetFile "sale")
             $(widgetFile "comments")
postOfferR :: OfferId -> Handler Html
postOfferR offerId = error "Not yet implemented: postOfferR"

--Comment
--    postTime UTCTime
--    user UserId
--    offer OfferId
--    description Html

makeCommentForm :: Key User -> Key Offer -> Form Comment
makeCommentForm uid offer = renderDivs $ Comment
    <$> lift (liftIO getCurrentTime)
    <*> pure uid 
    <*> pure offer
    <*> areq nicHtmlField "Content" Nothing
