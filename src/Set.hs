{-# LANGUAGE MultiParamTypeClasses #-}
module Set where

  class Set s a where
      empty   :: s a
      isEmpty :: s a -> Bool
      elem    :: a -> s a -> Bool
      insert  :: a -> s a -> s a
