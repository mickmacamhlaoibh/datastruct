{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RedBlackTree(rbTest, fromList, toList, empty, isEmpty, elem, insert, min, max, next, prev, prettyShow) where
   
  import Set as S
  import Prelude hiding (elem, min, max)

  -- Test Code

  testSize :: Int
  testSize = 2000000

  testList :: [Int]
  testList = [1..testSize]
  
  rbTest :: IO ()
  rbTest = do
    putStrLn "Build set"
    let s = fromList testList
    -- print $ s
    putStrLn $ "Set built from " ++  show testSize ++" elements"
    let e1 = 42567
    putStrLn $ "Find elem " ++ show e1
    print $ elem e1 s
    let e2 = 999999942567 :: Int
    putStrLn $ "Find non-elem: " ++ show e2
    print $ elem e2 s
    putStrLn $ "Find all elems from 1 to " ++ show testSize
    let find = flip elem s
    print $ all find [1..testSize]
    putStrLn "Write index to file"
    writeFile "redblack.txt" (show s)
    putStrLn "Read index from file"
    index <- readFile "redblack.txt"
    let s' = read index :: RBTree Int
    putStrLn $ "Find elem in index read from file: " ++ show e1
    print $ elem 42567 s'
    putStrLn "Finished"


  -- Main Code
  
  data Colour = R | B deriving (Eq, Read, Show)

  data RBTree a = Nil | Node Colour a (RBTree a) (RBTree a) deriving (Eq, Read, Show)

  prettyShow  :: (Show a) => RBTree a -> String
  prettyShow Nil = "E"
  prettyShow (Node c x l r) = unlines (ppHelper (Node c x l r)) where
    
    ppHelper :: Show a => RBTree a -> [String]
    ppHelper Nil = []
    ppHelper (Node c' x' l' r') = (show x' ++ show c') : ppSubtree l' r'
    
    ppSubtree :: Show a => RBTree a -> RBTree a -> [String]
    ppSubtree l' r' = pad "<- " "|  " (ppHelper l') ++ pad ">- " "   " (ppHelper r')
      
    pad :: String -> String -> [String] -> [String]
    pad first rest = zipWith (++) (first : repeat rest)
  
  toList :: RBTree a -> [a]
  toList Nil = []
  toList (Node _ x l r) = toList l ++ [x] ++ toList r
  
  fromList :: Ord a => [a] -> RBTree a
  fromList = foldr insert Nil

  min :: RBTree a -> Maybe a
  min Nil = Nothing
  min (Node _ x Nil _) = Just x
  min (Node _ _ l _) = min l
      
  max :: RBTree a -> Maybe a
  max Nil = Nothing
  max (Node _ x _ Nil) = Just x
  max (Node _ _ _ r) = max r
    
  prev :: Ord a => a -> RBTree a -> Maybe a
  prev x s = prev' s Nothing where
    prev' Nil p = p
    prev' (Node _ y l r) p
        | x <= y = prev' l p
        | x > y && isEmpty r = Just y
        | otherwise = prev' r (Just y)
    
  next :: Ord a => a -> RBTree a -> Maybe a
  next x s = next' s Nothing where
    next' Nil p = p
    next' (Node _ y l r) p
        | x >= y = next' r p
        | x < y && isEmpty l = Just y
        | otherwise = next' l (Just y)

  instance Ord a => Set RBTree a where
      
    empty = Nil
      
    isEmpty Nil = True
    isEmpty _   = False

    elem _ Nil = False
    elem x (Node _ y l r)
      | x < y = S.elem x l
      | x > y = S.elem x r
      | otherwise = True

    insert x s = makeBlack $ ins s where
      makeBlack :: RBTree a -> RBTree a 
      makeBlack Nil = Nil
      makeBlack (Node _ y l r) = Node B y l r 

      -- ins :: RBTree a -> RBTree a
      ins Nil = Node R x Nil Nil
      ins s'@(Node c y l r)
        | x < y = lBalance c y (ins l) r
        | x > y = rBalance c y l (ins r)
        | otherwise = s'

  -- Helper functions
  lBalance:: Colour -> a -> RBTree a -> RBTree a -> RBTree a
  lBalance B z (Node R y (Node R x a b) c) d = Node R y (Node B x a b) (Node B z c d)
  lBalance B z (Node R x a (Node R y b c)) d = Node R y (Node B x a b) (Node B z c d)
  lBalance colour x l r = Node colour x l r 

  rBalance:: Colour -> a -> RBTree a -> RBTree a -> RBTree a
  rBalance B x a (Node R z (Node R y b c) d) = Node R y (Node B x a b) (Node B z c d)
  rBalance B x a (Node R y b (Node R z c d)) = Node R y (Node B x a b) (Node B z c d)
  rBalance colour x l r = Node colour x l r 