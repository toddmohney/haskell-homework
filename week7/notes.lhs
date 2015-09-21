Monoids & newtype

Monoid Laws:
1. Has a defined identity value
2. Two values of the monoid can be combined to make another value of the same type
3. The combinatory function is associative

  class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m

    mappend :: m -> m -> m
    (<>) = mappend

    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty 

Examples:
  Lists
    identity           = ???
    combining function = ???
    associativity      = ???

>  import Data.Monoid
>  import Control.Applicative
>
>  newtype MyList a = MyList [a] deriving (Eq, Show, Ord)
>
>  instance Monoid (MyList a) where
>    mempty = MyList []
>    (MyList a) `mappend` (MyList b) = MyList (a ++ b)
>
>  getList :: MyList a -> [a]
>  getList (MyList a) = a


  Real Numbers
    - (addition)
      identity           = ???
      combining function = ???
      associativity      = ???

    - (multiplication)
      identity            = ???
      combining funcation = ???
      associativity       = ???

    - subtraction?
      identity            = ???
      combining funcation = ???
      associativity       = ???

    - division?
      identity            = ???
      combining funcation = ???
      associativity       = ???



  Bools
    identity           = ???
    combining function = ???
    associativity      = ???


Why is this useful?
- Foldable (https://www.fpcomplete.com/user/mgsloan/monoids-tour#foldable)
- Writer Monad (http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html)
- Tree Traversal (http://apfelmus.nfshost.com/articles/monoid-fingertree.html)

