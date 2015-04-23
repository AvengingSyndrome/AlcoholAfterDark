module Handler.AcceptOffer where

import Import

postAcceptOfferR :: ProposalId -> OfferId -> Handler Html
postAcceptOfferR proposalId offerId = do
  aid <- requireAuthId
  prop <- runDB $ get404 $ proposalId
  off <- runDB $ get404 $ offerId
  if (aid /= proposalPosterUser prop || proposalSold proposal /= True) 
    then do 
      setMessage "You can't post here!"
      redirect $  SellR proposalId 
    else do 
      {- 
         Now we run a whole bunch of paypal stuff here?
      -}
     error "implement me" 
      
