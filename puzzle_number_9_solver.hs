{-# OPTIONS_GHC -O2 #-}
import Data.List (sortBy,nubBy)
import Data.Ord (compare)
import System.Time

main = do  
    putStrLn "Puzzle Number 9 Solver Copyright May 2015 alhambra1"
    putStrLn "\nEnter 'e' at any time to exit"
    putStrLn "\nEnter target number"
    target <- getLine  
    if null target  
        then main
        else if head target == 'e'    
                then return ()        
                else do 
                      putStrLn "Enter number of moves at each choice point (density, 3 to 6 recommended)"  
                      density <- getLine  
                      if null density  
                          then main
                          else if head density == 'e'    
                                  then return ()        
                                  else do 
                                        putStrLn "Enter board numbers separated by spaces"  
                                        board <- getLine  
                                        if null board  
                                            then main
                                            else if head board == 'e'    
                                                    then return ()        
                                                    else do 
                                                        putStrLn ""
                                                        time1 <- getClockTime 
                                                        let b = map (\x -> read x :: Int) (take 9 $ words board)
                                                            t = read (head (words target)) :: Int
                                                            d = read (head (words density)) :: Int
                                                        print (map reverse $ reverse $ head $ take 1 $ f t b [] d)
                                                        time2 <- getClockTime
                                                        putStrLn ""
                                                        print (timeDiffToString $ diffClockTimes time2 time1)
                                                        putStrLn ""
                                                        exit

exit = do
     putStrLn "Enter 'a' to start again or 'e' to exit"
     line <- getLine
     if null line 
        then exit
             else if head line == 'a'
                     then do putStrLn ""
                             main
                          else if head line == 'e'
                                  then return ()
                                  else exit

f target board paths toTake
  | not (null hasTarget) = [(((\(x,y,z)-> z) . head $ hasTarget):paths)]
  | null ms              = []
  | otherwise            = do (s,bd,idxs) <- take toTake (sortBy (\(x,y,z) (x',y',z') -> compare x' x) ms')
                              f target bd (idxs:paths) toTake
 where hasTarget = filter ((==target) . (\(x,y,z)-> x)) ms
       ms = moves board
       ms' = nubBy (\(x,y,z) (x',y',z') -> let a = drop 1 (init z)
                                               b = drop 1 (init z')
                                           in if not (null a) && not (null b)
                                                 then a == b
                                                 else False) ms

moves board = do j <- [1..9]
                 let num = board !! (j - 1)
                     board' = (take (j - 1) board) ++ [num + 1] ++ (drop j board)
                 moves' j board' num [j] 0 num
 where moves' ix board s idxs prev next
        | (s == 9 || multiple) && (length idxs > 1) = [(s,board',idxs)]
        | s > 9 && mod s 9 /= 0 = []
        | otherwise = case ix of
            1 -> if elem 2 idxs then [] else moves' 2 board' (s + b) (2:idxs) next b
                 ++ (if elem 4 idxs then [] else moves' 4 board' (s + d) (4:idxs) next d)
                 ++ (if elem 5 idxs then [] else moves' 5 board' (s + e) (5:idxs) next e)
            2 -> if elem 1 idxs then [] else moves' 1 board' (s + a) (1:idxs) next a
                 ++ (if elem 3 idxs then [] else moves' 3 board' (s + c) (3:idxs) next c)
                 ++ (if elem 4 idxs then [] else moves' 4 board' (s + d) (4:idxs) next d)
                 ++ (if elem 5 idxs then [] else moves' 5 board' (s + e) (5:idxs) next e)
                 ++ (if elem 6 idxs then [] else moves' 6 board' (s + f) (6:idxs) next f)
            3 -> if elem 2 idxs then [] else moves' 2 board' (s + b) (2:idxs) next b
                 ++ (if elem 5 idxs then [] else moves' 5 board' (s + e) (5:idxs) next e)
                 ++ (if elem 6 idxs then [] else moves' 6 board' (s + f) (6:idxs) next f)
            4 -> if elem 1 idxs then [] else moves' 1 board' (s + a) (1:idxs) next a
                 ++ (if elem 2 idxs then [] else moves' 2 board' (s + b) (2:idxs) next b)
                 ++ (if elem 5 idxs then [] else moves' 5 board' (s + e) (5:idxs) next e)
                 ++ (if elem 7 idxs then [] else moves' 7 board' (s + g) (7:idxs) next g)
                 ++ (if elem 8 idxs then [] else moves' 8 board' (s + h) (8:idxs) next h)
            5 -> if elem 1 idxs then [] else moves' 1 board' (s + a) (1:idxs) next a
                 ++ (if elem 2 idxs then [] else moves' 2 board' (s + b) (2:idxs) next b)
                 ++ (if elem 3 idxs then [] else moves' 3 board' (s + c) (3:idxs) next c)
                 ++ (if elem 4 idxs then [] else moves' 4 board' (s + d) (4:idxs) next d)
                 ++ (if elem 6 idxs then [] else moves' 6 board' (s + f) (6:idxs) next f)
                 ++ (if elem 7 idxs then [] else moves' 7 board' (s + g) (7:idxs) next g)
                 ++ (if elem 8 idxs then [] else moves' 8 board' (s + h) (8:idxs) next h)
                 ++ (if elem 9 idxs then [] else moves' 9 board' (s + i) (9:idxs) next i)
            6 -> if elem 2 idxs then [] else moves' 2 board' (s + b) (2:idxs) next b
                 ++ (if elem 3 idxs then [] else moves' 3 board' (s + c) (3:idxs) next c)
                 ++ (if elem 5 idxs then [] else moves' 5 board' (s + e) (5:idxs) next e)
                 ++ (if elem 8 idxs then [] else moves' 8 board' (s + h) (8:idxs) next h)
                 ++ (if elem 9 idxs then [] else moves' 9 board' (s + i) (9:idxs) next i)
            7 -> if elem 4 idxs then [] else moves' 4 board' (s + d) (4:idxs) next d
                 ++ (if elem 5 idxs then [] else moves' 5 board' (s + e) (5:idxs) next e)
                 ++ (if elem 8 idxs then [] else moves' 8 board' (s + h) (8:idxs) next h)
            8 -> if elem 4 idxs then [] else moves' 4 board' (s + d) (4:idxs) next d
                 ++ (if elem 5 idxs then [] else moves' 5 board' (s + e) (5:idxs) next e)
                 ++ (if elem 6 idxs then [] else moves' 6 board' (s + f) (6:idxs) next f)
                 ++ (if elem 7 idxs then [] else moves' 7 board' (s + g) (7:idxs) next g)
                 ++ (if elem 9 idxs then [] else moves' 9 board' (s + i) (9:idxs) next i)
            9 -> if elem 5 idxs then [] else moves' 5 board' (s + e) (5:idxs) next e
                 ++ (if elem 6 idxs then [] else moves' 6 board' (s + f) (6:idxs) next f)
                 ++ (if elem 8 idxs then [] else moves' 8 board' (s + h) (8:idxs) next h)
          where multiple = length idxs == 2 && prev == next && mod s 9 == 0
                [a,b,c,d,e,f,g,h,i] = board
                board' = if s == 9
                            then (take (headIdxs - 1) board) ++ [9] ++ (drop headIdxs board)
                            else if multiple
                                    then board''
                                    else (take (headIdxs - 1) board) ++ [next + 1] ++ (drop headIdxs board)
                board'' = map (\(x,y) -> if x == headIdxs then y * 2 else if x == last idxs then 1 else y) (zip [1..] board)
                headIdxs = head idxs
