module JoinList where
  import Data.Monoid
  import Sized

  data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

  (+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
  a +++ Empty = a
  Empty +++ b = b
  a +++ b     = Append (tag a `mappend` tag b) a b

  tag :: Monoid m => JoinList m a -> m
  tag Empty = mempty
  tag (Single m _) = m
  tag (Append m _ _) = m

  indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
  indexJ _ Empty = Nothing
  indexJ 0 (Single m a) = Just a
  indexJ _ (Single _ _) = Nothing
  indexJ i (Append _ l r)
    | i < sizeOfLeft   = indexJ i l
    | otherwise        = indexJ (i-sizeOfLeft) r
      where sizeOfLeft = getSize . size . tag $ l

  (!!?) :: [a] -> Int -> Maybe a
  [] !!? _ = Nothing
  _ !!? i | i < 0 = Nothing
  (x:xs) !!? 0 = Just x
  (x:xs) !!? i = xs !!? (i-1)

  jlToList :: JoinList m a -> [a]
  jlToList Empty = []
  jlToList (Single _ a) = [a]
  jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
