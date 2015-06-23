module Party where
  import Data.Monoid
  import Data.Tree
  import Employee
  import System.Environment(getArgs)

  instance Monoid GuestList where
    mempty = GL [] 0
    mappend = addLists

  addLists :: GuestList -> GuestList -> GuestList
  addLists (GL empList1 listFun1) (GL empList2 listFun2) = 
    GL (empList1 ++ empList2) (listFun1 + listFun2)

  glCons :: Employee -> GuestList -> GuestList
  glCons employee@(Emp empName empFun) (GL empList listFun) = 
    GL (employee:empList) (listFun + empFun)

  moreFun :: GuestList -> GuestList -> GuestList
  moreFun list1@(GL _ fun1) list2@(GL _ fun2)
    | fun1 >= fun2 = list1
    | otherwise    = list2

  nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
  nextLevel boss []     = ((glCons boss mempty), mempty)
  nextLevel boss gLists = (withBoss, withoutBoss)
    where withBoss    = glCons boss (mconcat (map fst gLists))
          withoutBoss = mconcat (map snd gLists)

  treeFold :: (a -> [b] -> b) -> Tree a -> b
  treeFold f (Node root xs) = f root (map (treeFold f) xs)

  maxFun :: Tree Employee -> GuestList
  maxFun tree = (uncurry moreFun (treeFold nextLevel tree))

  parse :: GuestList -> String
  parse (GL emp fun) = "Fun score : " 
                      ++ show fun ++ "\n"
                      ++ unlines (map empName emp)
  main :: IO ()
  main = readFile "company.txt" >>= putStrLn . parse . maxFun . read

