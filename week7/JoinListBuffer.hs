module JoinListBuffer where
  import Buffer
  import Scrabble
  import Sized
  import JoinList

  data EditorJoinList = JoinList (Score, Size) String

  instance Buffer EditorJoinList where
    toString buf            = undefined
    fromString str          = undefined
    line num                = undefined
    replaceLine num str buf = undefined
    numLines buf            = undefined
    value buf               = undefined
