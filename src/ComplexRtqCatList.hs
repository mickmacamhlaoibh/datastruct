module ComplexRtqCatList(clTest, CatList, RTQ.RTQueue, empty, isEmpty, (>:), (<:), (+++), hd, tl, hdU, tlU, toList) where
  
  import CatenableList
  import Queue(Queue)
  import qualified RealTimeQueue as RTQ
  
  -- Test Code
  data Test = CatList | Standard deriving Show
  
  whichTest :: Test
  whichTest = CatList

  testSize :: Int
  testSize = 20000000
  
  clTest :: IO ()
  clTest = do
    putStrLn ("Start Test: " ++ show whichTest)
    print (case whichTest of 
      CatList  -> testCatList 
      Standard -> testStdList)
    putStrLn ("End Test: " ++ show whichTest)
  
  testData :: [Int]
  testData = [1..testSize]
  
  testCatList :: Maybe Int
  testCatList =  hd l' where
    -- add all elements in test list to the end of cat list
    l = foldl (<:) E testData :: CatList RTQ.RTQueue Int 
    -- get the last element of the list by repeatedly calling tail for the testSize minus one
    l' = dropC (testSize-1) l
    
  testStdList :: Maybe Int
  testStdList = Just (head l') where
    -- add all elements in test list to the end of cat list
    snoc xs x = xs ++ [x]
    l = foldl snoc [] testData 
    -- get the last element of the list by repeatedly calling tail for the testSize minus one
    l' = drop (testSize-1) l
  
  dropC :: Queue q => Int -> CatList q a -> CatList q a
  dropC 0 l = l
  dropC n l 
    | isEmpty l = l
    | otherwise = dropC (n-1) (tlU l)
      
  -- Main Code
  data CatList q a = E | C a (q (CatList q a))
  
  toList :: Queue q => CatList q a -> [a]
  toList l
    | isEmpty l = []
    | otherwise = hdU l:toList (tlU l)
  
  instance Queue q => CatenableList (CatList q) where
    
    empty = E
      
    isEmpty E = True
    isEmpty _ = False
    
    -- cons: add to front
    x >: xs = C x RTQ.empty +++ xs
    
    -- snoc: add to end
    xs <: x = xs +++ C x RTQ.empty
    
    -- append
    xs +++ E  = xs
    E  +++ ys = ys
    xs +++ ys = link xs ys
    
    -- safe head
    hd E = Nothing
    hd (C x _) = Just x
    
    -- safe tail   
    tl E = Nothing
    tl (C _ q)
      | RTQ.isEmpty q = Just E
      | otherwise   = Just (linkAll q)
        where
          linkAll q'
            | RTQ.isEmpty q'' = t
            | otherwise = link t (linkAll q'')
                where
                  q'' = RTQ.popU q'
                  t   = RTQ.topU q'
    
    -- Unsafe head
    hdU E = error "Empty catenable list"
    hdU (C x _) = x
    
    -- Unsafe tail   
    tlU (C _ q)
      | RTQ.isEmpty q = E
      | otherwise   = linkAll q
        where
          linkAll q'
            | RTQ.isEmpty q'' = t
            | otherwise = link t (linkAll q'')
                where
                  q'' = RTQ.popU q'
                  t   = RTQ.topU q'
    tlU E = error "Empty catenable list"

  -- Helper function
  link :: Queue q => CatList q a -> CatList q a -> CatList q a
  link (C x q) s = C x (RTQ.add s q)
  -- Can never happen - just here to stop warnings
  link E s = s
