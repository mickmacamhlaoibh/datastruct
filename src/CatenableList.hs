module CatenableList(CatenableList(..)) where

  class CatenableList c where
    empty :: c a
    isEmpty  :: c a -> Bool
    
    -- cons: add to front
    (>:)  :: a   -> c a -> c a
    -- snoc: add to end
    (<:)  :: c a -> a   -> c a
    -- append
    (+++) :: c a -> c a -> c a 
    
    -- safe head
    hd    :: c a -> Maybe a
    -- safe tail   
    tl    :: c a -> Maybe (c a)
    
    -- These are useful when you know for sure that the queue is not empty and you want to avoid all the
    -- unwrapping and re-wrapping of the Maybe 
    -- A good example is where we implement the drop function forCatenableLists in RtqCatList
    -- Unsafe head
    hdU  :: c a -> a
    -- Unsafe tail   
    tlU   :: c a -> c a
