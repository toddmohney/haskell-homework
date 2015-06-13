module JoinList where
  import Data.Monoid

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
