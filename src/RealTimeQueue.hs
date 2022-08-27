module RealTimeQueue(rtqTest, RTQueue, fromList, dropQ, empty, isEmpty, add, top, pop, topU, popU, cons) where
  
  import Queue
  
  -- Test Code
  rtqTest :: IO ()
  rtqTest = do
    putStrLn "Start RTQ Test"
    print test
    putStrLn "End RTQ Test"
    
  test :: [Int]
  test = testBatch 100000 10000000 1 empty where
    testBatch n m s q 
      | s < m      = x:testBatch n m s' q''  
      | otherwise  = []
        where
          e  = s + n
          q'  = addFromList q [s..e]
          q'' = dropQ (n-10000) q'
          x   = topU q''
          s'  = e + 1
  
  -- Main Code
  data RTQueue a = RTQ [a] [a] [a] 
  
  fromList :: [a] -> RTQueue a
  fromList = addFromList empty
  
  addFromList :: RTQueue a -> [a] -> RTQueue a
  addFromList = foldl (flip add) 
  
  dropQ :: Int -> RTQueue a -> RTQueue a
  dropQ 0 q = q
  dropQ _ q@(RTQ [] _ _ ) = q
  dropQ n q = dropQ (n-1) (popU q)

  cons :: a -> RTQueue a -> RTQueue a
  cons x (RTQ f r s)  = RTQ (x:f) r s
  
  instance Queue RTQueue where
    
    empty = RTQ [] [] []
    
    isEmpty (RTQ [] _ _) = True
    isEmpty _            = False

    add x (RTQ f r s) = exec f (x:r) s
    
    top (RTQ [] _ _) = Nothing
    top (RTQ (x:_) _ _ ) = Just x
    
    pop (RTQ [] _ _ ) = Nothing
    pop (RTQ (_:f) r s) = Just (exec f r s) 
    
    topU (RTQ [] _ _) = error "Empty queue"
    topU (RTQ (x:_) _ _ ) = x
    
    popU (RTQ (_:f) r s) = exec f r s 
    popU (RTQ [] _ _ ) = error "Empty queue" 
      
  -- Helper functions
  exec :: [a] -> [a] -> [a] -> RTQueue a
  exec f r [] = RTQ f' [] f' where f' = rotate f r []
  exec f r (_:s) = RTQ f r s
  
  rotate :: [a] -> [a] -> [a] -> [a]
  rotate [] (y:_) a = y:a
  rotate (x:xs) (y:ys) a = x:rotate xs ys (y:a)
  -- Can never happen - just here to turn off the warnings
  rotate _ _ _ = [] 