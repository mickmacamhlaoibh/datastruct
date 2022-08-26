module Perm(permTest, perms, permsC) where
  
  import RtqCatList
  
  -- Test Code
  
  permTest :: IO ()
  permTest = do
    putStrLn "Start perm test"
    print $ length $ perms 10
    putStrLn "End perm test"
  
  -- Main Code
  perms :: Int -> [[Int]]
  perms 0 = [[]]
  perms n = concatMap (rotations n) (perms (n-1))
    
  permsC :: Int -> [CatList RTQueue Int]
  permsC 0 = [empty]
  permsC n = concatMap (rotationsC n) (permsC (n-1))
     
  rotations :: Int -> [Int] -> [[Int]] 
  rotations n ns = rot n (n:ns) where
    rot 0 ms = [ms] 
    rot i ms = ms:rot (i-1) (rotate ms)
  
  rotate :: [a] -> [a]
  rotate [] = []
  rotate (x:xs) = xs `snoc` x
    
  snoc :: [a] -> a -> [a]
  snoc [] x = [x]
  snoc xs x = xs ++ [x]
     
  rotationsC :: Int -> CatList RTQueue Int -> [CatList RTQueue Int] 
  rotationsC n ns = rot n (n>:ns) where
    rot 0 ms = [ms] 
    rot i ms = ms : rot (i-1) (rotateC ms)
  
  rotateC :: CatList RTQueue s -> CatList RTQueue s
  rotateC xs
    | isEmpty xs = xs
    | otherwise  = tlU xs <: hdU xs
