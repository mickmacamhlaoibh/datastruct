module RtqCatList(empty, isEmpty, (>:), (<:), (+++), hd, tl, hdU, tlU) where
  
  import CatenableList
  import qualified RealTimeQueue as RTQ
  
  instance CatenableList RTQ.RTQueue where
    empty = RTQ.empty
    isEmpty  = RTQ.isEmpty
    
    -- cons: add to front
    (>:)  = RTQ.cons
    -- snoc: add to end
    (<:)  = flip RTQ.add
    
    -- append
    xs +++ ys
      | RTQ.isEmpty ys = xs
      | RTQ.isEmpty xs = ys
      | otherwise = xs -- TODO link xs ys
    
    -- safe head
    hd = RTQ.top
    -- safe tail   
    tl = RTQ.pop
    
    -- These are useful when you know for sure that the list is not empty and you want to avoid all the
    -- unwrapping and re-wrapping of the Maybe 
    -- Unsafe head
    hdU = RTQ.topU
    -- Unsafe tail   
    tlU = RTQ.popU
    
    

