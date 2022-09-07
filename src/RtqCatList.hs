module RtqCatList(clTest, CatList, RTQ.RTQueue, empty, isEmpty, (>:), (<:), (+++), hd, tl, hdU, tlU, toList) where
  
  import CatenableList
  import Queue(Queue)
  import qualified RealTimeQueue as RTQ
  -- import GHC.List (foldl')
  
  -- Test Code
  data Test = SnocCatList | SnocStandard | AppendCatList | AppendStandard deriving Show
  
  whichTest :: Test
  whichTest = SnocStandard
  
  clTest :: IO ()
  clTest = do
    putStrLn ("Start Test: " ++ show whichTest)
    print 
      (
        case whichTest of 
          SnocCatList  -> testSnocCatList 
          SnocStandard -> testSnocStdList
          AppendCatList  -> testAppendCatList 
          AppendStandard -> testAppendStdList
      )
    putStrLn ("End Test: " ++ show whichTest)

  testSnocSize :: Int
  testSnocSize = 20000000
  
  testSnocData :: [Int]
  testSnocData = [1..testSnocSize]
  
  testSnocCatList :: Maybe Int
  testSnocCatList =  hd l' where
    -- add all elements in test list to the end of the cat list
    l = foldl (<:) E testSnocData :: CatList RTQ.RTQueue Int 
    -- get the last element of the list by repeatedly calling tail for the testSnocSize minus one
    l' = dropC (testSnocSize-1) l
    
  testSnocStdList :: Maybe Int
  testSnocStdList = Just (head l') where
    -- add all elements in test list to the end of the standard list
    -- snoc xs x = xs ++ [x]
    -- l = foldl snoc [] testSnocData 
    -- snocR x xs = xs ++ [x]
    -- l = foldr snocR [] testSnocData
    l = concatMap (: []) testSnocData 
    -- get the last element of the list by repeatedly calling tail for the testSnocSize minus one
    l' = drop (testSnocSize-1) l

  testAppendSize :: Int
  testAppendSize = 10000000
  
  testAppendData :: [[Int]]
  testAppendData = splits 100 [1..testAppendSize] where
    splits _ [] = []
    splits n xs = take n xs:splits n (drop n xs)
    
  testAppendCatList :: Maybe Int
  testAppendCatList = hd l' where
    -- append all lists in the test list of lists to the end of the cat list
    ls = map (foldr (>:) E) testAppendData :: [CatList RTQ.RTQueue Int]
    l = foldr (+++) E ls
    -- get the last element of the list by repeatedly calling tail for the testSnocSize minus one
    l' = dropC (testAppendSize-1) l
    
  testAppendStdList :: Maybe Int
  testAppendStdList = Just (head l') where
    -- append all lists in the test list of lists to the end of the std list
    l = concat testAppendData 
    -- get the last element of the list by repeatedly calling tail for the testSnocSize minus one
    l' = drop (testAppendSize-1) l
  
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
