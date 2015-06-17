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
  a +++ b     = Append (tag a <> tag b) a b

  tag :: Monoid m => JoinList m a -> m
  tag Empty = mempty
  tag (Single m _) = m
  tag (Append m _ _) = m

  indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
  indexJ _ Empty = Nothing
  indexJ i (Single m a)
    | i == 0 = Just a
    | otherwise = Nothing
  indexJ i (Append _ l r)
    | i < (sizeJ l) = indexJ i l
    | otherwise     = indexJ (i-(sizeJ l)) r

  dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
  dropJ 0 jl = jl
  dropJ _ Empty = Empty
  dropJ _ (Single _ _) = Empty
  dropJ i (Append m l r)
    | i < (sizeJ l) = (dropJ i l) +++ r
    | otherwise     = dropJ (i - (sizeJ l)) r

  takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
  takeJ 0 _ = Empty
  takeJ _ jl@(Single m a) = jl
  takeJ i (Append m l r)
    | i <= (sizeJ l) = takeJ i l
    | otherwise     = l +++ (takeJ i r)

  sizeJ :: (Sized b, Monoid b) => JoinList b a -> Int
  sizeJ jl = getSize . size . tag $ jl

  (!!?) :: [a] -> Int -> Maybe a
  [] !!? _ = Nothing
  _ !!? i | i < 0 = Nothing
  (x:xs) !!? 0 = Just x
  (x:xs) !!? i = xs !!? (i-1)

  jlToList :: JoinList m a -> [a]
  jlToList Empty = []
  jlToList (Single _ a) = [a]
  jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
