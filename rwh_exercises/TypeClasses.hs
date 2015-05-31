module TypeClasses where

  data ResponseType = Success Int
                    | Error Int

  instance Show ResponseType where
    show (Success int) = "Success! " ++ (show int)
    show (Error int)   = "Error :( " ++ (show int)

  instance Eq ResponseType where
    (Success int1) == (Success int2) = int1 == int2
    (Error int1) == (Error int2)     = int1 == int2
    _ == _                           = False



  class YesNo a where
    yesNo :: a -> Bool

  instance YesNo Int where
    yesNo 0 = False
    yesNo _ = True

  instance YesNo Integer where
    yesNo 0 = False
    yesNo _ = True

  instance YesNo Double where
    yesNo 0 = False
    yesNo _ = True

  instance YesNo Float where
    yesNo 0 = False
    yesNo _ = True

  instance YesNo Bool where
    yesNo = id

  instance YesNo [a] where
    yesNo [] = False
    yesNo _  = True

  instance YesNo (Maybe a) where
    yesNo (Just _) = True
    yesNo Nothing  = False

  data ResultType = Yeah
                  | Nah
                  deriving(Show)

  yeahErrNah :: (YesNo a) => a -> b -> b -> b
  yeahErrNah yesNoVal yeahType nahType = if (yesNo yesNoVal) then yeahType else nahType

