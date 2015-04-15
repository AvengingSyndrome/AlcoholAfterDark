module Handler.Manage where

import Import

getManageR :: Handler Html
getManageR =  do
  (Entity _ u) <- requireAuth
  (profileWidget, enctype) <- generateFormPost $ userForm $ u
  defaultLayout $ do $(widgetFile "manage")  

postManageR :: Handler Html
postManageR = do 
    u <- requireAuthId
    (Entity _ user') <- requireAuth
    ((res,profileWidget),enctype) <- runFormPost $  userForm user'
    case res of
         FormSuccess user -> do
            _ <- runDB $ replace u user
            setMessage $ "Updated"
            redirect $ ManageR
         _ -> defaultLayout $ do
            setTitle "Please correct your entry form"
            $(widgetFile "manage") 

userForm :: User -> Form User
userForm user = renderDivs $ User
    <$> pure (userIdent user)
    <*> pure (userPassword user)
    <*> aopt textField "Display Name" (Just $ userName user)
    <*> aopt textField "Post Code"  (Just $ userPostcode user)
