# Funktionell programmering på Chalmers och GU

Presentatörer: Patrik Jansson och Emil Axelsson

## Innehåll:

Problemlösning med dator är det ni förväntas lära er de närmaste åren. Funktionell programmering är ett väldigt kraftfullt verktyg för sådan problemlösning. Vi kommer att presentera några exempel på resultat, projekt och kurser inom området som har sitt ursprung här i Göteborg.
```Haskell
sort :: [Int] -> [Int]
sort []      = []
sort (x:xs)  = sort smaller ++ [x] ++ sort larger
  where smaller = filter (<=x) xs
        larger  = filter (>x)  xs
```
