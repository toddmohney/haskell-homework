module Party where
  import Data.Monoid
  import Data.Tree
  import Employee

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

  treeFold :: (a -> b -> b) -> b -> Tree a -> b
  treeFold fun acc t = foldr fun acc (flatten t)

  nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
  nextLevel = undefined

  maxFun :: Tree Employee -> GuestList
  maxFun = undefined

