module Queue(Queue(..)) where
  
  class Queue q where
    empty   :: q a
    isEmpty :: q a -> Bool
    
    add     :: a -> q a -> q a
    
    -- Safe top and pop 
    top     :: q a -> Maybe a
    pop    :: q a -> Maybe (q a)

    -- Unsafe top and pop
    -- These are useful when you know for sure that the queue is not empty and you want to avoid all the
    -- unwrapping and re-wrapping of the Maybe 
    -- A good example is where we implement the tail function for CatenableLists using a RealTimeQueue in RtqCatList
    topU    :: q a -> a
    popU   :: q a -> q a
