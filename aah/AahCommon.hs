module AahCommon where
import Import
import Yesod.Form.Nic (nicHtmlField)

getUser :: Maybe (Entity a) -> Maybe a
getUser (Just (Entity _ u)) = Just u
getUser Nothing = Nothing

writeSale :: Key User -> Bool ->  Form Proposal
writeSale uid sale = renderDivs $ Proposal
    <$> lift (liftIO getCurrentTime)
    <*> areq doubleField "Price" Nothing
    <*> pure (uid)
    <*> pure Nothing
    <*> areq textField "Title" Nothing
    <*> areq boolField "Delivery?" Nothing
    <*> pure False
    <*> areq nicHtmlField "Content" Nothing
    <*> pure sale

saleOrBuyProposals :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => Bool -> HandlerT site IO [Entity Proposal]
saleOrBuyProposals sale = runDB $ selectList [ProposalSale ==. sale] [Desc ProposalPostTime]
 
getSales :: Bool -> Handler Html
getSales sale = do
    maid' <- maybeAuthId
    sells <- saleOrBuyProposals sale
    case maid' of
     Just maid -> do (saleWidget, enctype) <- generateFormPost $ writeSale maid sale
                     defaultLayout $ do 
                      $(widgetFile "saleForm")  
                      $(widgetFile "aSales")
     Nothing -> do defaultLayout $ do 
                    $(widgetFile "aSales")
