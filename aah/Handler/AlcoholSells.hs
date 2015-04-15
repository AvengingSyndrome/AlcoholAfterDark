module Handler.AlcoholSells where

import Import
import AahCommon
getAlcoholSellsR :: Handler Html
getAlcoholSellsR = getSales True 


postAlcoholSellsR :: Handler Html
postAlcoholSellsR = do
    u <- requireAuthId
    let sale = True
    ((res,saleWidget),enctype) <- runFormPost $  writeSale u True 
    case res of
         FormSuccess sell -> do
            _ <- runDB $ insert sell
            sells <- saleOrBuyProposals True   
            setMessage $ "Posted!"
            defaultLayout $ do
             $(widgetFile "saleForm")  
             $(widgetFile "aSales")
         _ ->  do sells <- saleOrBuyProposals True 
                  defaultLayout $ do
                   setTitle "Please correct your entry form"
                   $(widgetFile "saleForm")  
                   $(widgetFile "aSales")

