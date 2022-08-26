{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BinaryTree (fromList, toList, empty, isEmpty, elem, insert, min, max, next, prev, show) where
  
  import Set as S
  import Prelude hiding (elem, min, max)
    
  data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Read)
    
  toList :: Tree a -> [a]
  toList Nil = []
  toList (Node x l r) = toList l ++ [x] ++ toList r
    
  fromList :: Ord a => [a] -> Tree a
  fromList = foldr insert Nil
    
  instance (Show a) => Show (Tree a) where
    show Nil = "*"
    show (Node x l r) = unlines (ppHelper (Node x l r)) where
      
      ppHelper :: Show a => Tree a -> [String]
      ppHelper Nil = []
      ppHelper (Node x' l' r') = show x' : ppSubtree l' r'
      
      ppSubtree :: Show a => Tree a -> Tree a -> [String]
      ppSubtree l' r' = pad "L- " "|  " (ppHelper l') ++ pad "R- " "   " (ppHelper r')
      
      pad :: String -> String -> [String] -> [String]
      pad first rest = zipWith (++) (first : repeat rest)
      
  min :: Tree a -> Maybe a
  min Nil = Nothing
  min (Node x Nil _) = Just x
  min (Node _ l _) = min l
    
  max :: Tree a -> Maybe a
  max Nil = Nothing
  max (Node x _ Nil) = Just x
  max (Node _ _ r) = max r
  
  prev :: Ord a => a -> Tree a -> Maybe a
  prev x s = prev' s Nothing where
    prev' Nil p = p
    prev' (Node y l r) p
      | x <= y = prev' l p
      | x > y && isEmpty r = Just y
      | otherwise = prev' r (Just y)
  
  next :: Ord a => a -> Tree a -> Maybe a
  next x s = next' s Nothing where
    next' Nil p = p
    next' (Node y l r) p
      | x >= y = next' r p
      | x < y && isEmpty l = Just y
      | otherwise = next' l (Just y)
      
      
  instance Ord a => Set Tree a where
    empty = Nil
    
    isEmpty Nil = True
    isEmpty _   = False
          
    elem _ Nil = False
    elem x (Node y l r)
      | x < y = S.elem x l
      | x > y = S.elem x r
      | otherwise = True
    
    insert x Nil = Node x Nil Nil
    insert x t@(Node y l r) 
      | x < y = insert x l
      | x > y = insert x r
      | otherwise = t 