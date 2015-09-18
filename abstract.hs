sort :: [Int] -> [Int]
sort []      = []
sort (x:xs)  = sort smaller ++ [x] ++ sort larger
  where smaller = filter (<x) xs
        larger  = filter (>=x)  xs
