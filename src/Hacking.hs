module Main where

import Data.Char
import System.Random
import System.IO

computeSuccess :: String -> String -> Int
computeSuccess key@(x:xs) attempt@(y:ys) 
    | length key /= length attempt = 0
    | length key == length attempt = compute key attempt
    where compute k a = foldl (\c (x,y) -> if x == y then c + 1 else c) 0 (zip k a)

toLowerString :: String -> String
toLowerString = map toLower

genGame :: [String] -> Int -> IO (String, [String])
genGame words diff = do
    gen <- getStdGen
    let filterList = filter (\x -> length x == (3 + diff)) words
        key = filterList !! fst (randomR (0, length filterList) gen)
        randomIndices = take (4 + diff) (randomRs (0, length filterList) gen)
        randomList = map (\x -> filterList !! x) randomIndices
    return (key, randomList)

playGame :: (String, [String]) -> IO ()
playGame game = playGame' game 4

playGame' :: (String, [String]) -> Int -> IO ()
playGame' _ 0 = putStrLn "ILLEGAL ACCESS DETECTED! TERMINAL LOCK-OUT ENGAGED"
playGame' game@(key, choices) n = do
    putStrLn "Guess password:"
    putStr $ unlines choices
    attempt <- getLine
    if attempt == key
    then putStrLn "Success! Terminal Access Granted!"
    else do
        let matches = computeSuccess key attempt
        putStrLn $ show matches ++ "/" ++ show (length key) ++ " correct"
        playGame' game (n - 1)

main :: IO ()
main = do 
    wordlist <- readFile "/usr/share/dict/words"
    let words = lines wordlist
    putStrLn "Select Difficulty (1-5):"
    diffLine <- getLine
    let diff = read diffLine
    game <- genGame words diff
    playGame game
